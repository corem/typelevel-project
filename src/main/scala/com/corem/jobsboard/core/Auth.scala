package com.corem.jobsboard.core

import cats.*
import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.Logger

import tsec.authentication.AugmentedJWT
import tsec.mac.jca.HMACSHA256
import tsec.authentication.IdentityStore
import scala.concurrent.duration.*
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash
import tsec.authentication.JWTAuthenticator
import tsec.authentication.BackingStore
import tsec.common.SecureRandomId
import cats.data.OptionT

import com.corem.jobsboard.domain.security.*
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.domain.user.*
import com.corem.jobsboard.config.*

trait Auth[F[_]] {
  def login(email: String, password: String): F[Option[JwtToken]]
  def signUp(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(
      email: String,
      newPasswordInfo: NewPasswordInfo
  ): F[Either[String, Option[User]]]
  def delete(email: String): F[Boolean]

  def authenticator: Authenticator[F]
}

class LiveAuth[F[_]: Async: Logger] private (
    users: Users[F],
    override val authenticator: Authenticator[F]
) extends Auth[F] {
  override def login(email: String, password: String): F[Option[JwtToken]] =
    for {
      maybeUser <- users.find(email)
      maybeValidatedUser <- maybeUser.filterA(user =>
        BCrypt
          .checkpwBool[F](
            password,
            PasswordHash[BCrypt](user.hashedPassword)
          )
      )
      maybeJwtToken <- maybeValidatedUser.traverse(user => authenticator.create(user.email))
    } yield maybeJwtToken

  override def signUp(newUserInfo: NewUserInfo): F[Option[User]] =
    for {
      mayberUser <- users.find(newUserInfo.email)
      result <- mayberUser match {
        case Some(_) => None.pure[F]
        case None =>
          for {
            hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
            user <- User(
              newUserInfo.email,
              hashedPassword,
              newUserInfo.firstName,
              newUserInfo.lastName,
              newUserInfo.company,
              Role.RECRUITER
            ).pure[F]
            _ <- users.create(user)
          } yield Some(user)
      }
    } yield result

  override def changePassword(
      email: String,
      newPasswordInfo: NewPasswordInfo
  ): F[Either[String, Option[User]]] =
    def updateUser(user: User, newPassword: String): F[Option[User]] =
      for {
        hashedPassword <- BCrypt.hashpw[F](newPasswordInfo.newPassword)
        updatedUser    <- users.update(user.copy(hashedPassword = hashedPassword))
      } yield updatedUser

    def checkAndUpdate(
        user: User,
        oldPassword: String,
        newPassword: String
    ): F[Either[String, Option[User]]] = {
      for {
        passCheck <- BCrypt
          .checkpwBool[F](
            newPasswordInfo.oldPassword,
            PasswordHash[BCrypt](user.hashedPassword)
          )
        updateResult <-
          if (passCheck) {
            updateUser(user, newPassword).map(Right(_))
          } else {
            Left("Invalid password").pure[F]
          }
      } yield updateResult
    }

    users.find(email).flatMap {
      case None => Right(None).pure[F]
      case Some(user) =>
        val NewPasswordInfo(oldPassword, newPassword) = newPasswordInfo
        checkAndUpdate(user, oldPassword, newPassword)

    }

  override def delete(email: String): F[Boolean] =
    users.delete(email)
}

object LiveAuth {
  def apply[F[_]: Async: Logger](
      users: Users[F]
  )(securityConfig: SecurityConfig): F[LiveAuth[F]] = {

    val idStore: IdentityStore[F, String, User] = (email: String) => OptionT(users.find(email))

    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
      new BackingStore[F, SecureRandomId, JwtToken] {
        override def get(id: SecureRandomId): OptionT[F, JwtToken] = OptionT(ref.get.map(_.get(id)))

        override def put(elem: JwtToken): F[JwtToken] =
          ref.modify(store => (store + (elem.id -> elem), elem))

        override def update(v: JwtToken): F[JwtToken] = put(v)

        override def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))
      }
    }

    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8"))

    for {
      key        <- keyF
      tokenStore <- tokenStoreF
      authenticator = JWTAuthenticator.backed.inBearerToken(
        expiryDuration = securityConfig.jwtExpiryDuration, // Token expiration
        maxIdle = None,                                    // Max idle time (optional)
        identityStore = idStore,                           // Id Store
        tokenStore = tokenStore,                           // Hash key
        signingKey = key
      )
    } yield new LiveAuth[F](users, authenticator)
  }
}
