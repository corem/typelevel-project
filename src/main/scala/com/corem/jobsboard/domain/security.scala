package com.corem.jobsboard.domain

import tsec.authentication.AugmentedJWT
import tsec.mac.jca.HMACSHA256
import tsec.authentication.JWTAuthenticator

import com.corem.jobsboard.domain.user.User
import tsec.authentication.SecuredRequest
import org.http4s.Response
import tsec.authorization.BasicRBAC
import com.corem.jobsboard.domain.user.*
import tsec.authorization.AuthorizationInfo
import cats.implicits.*
import cats.*
import tsec.authentication.{TSecAuthService, SecuredRequestHandler}
import org.http4s.Status

object security {
  type Crypto               = HMACSHA256
  type JwtToken             = AugmentedJWT[Crypto, String]
  type Authenticator[F[_]]  = JWTAuthenticator[F, String, User, Crypto]
  type AuthRoute[F[_]]      = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  type AuthRBAC[F[_]]       = BasicRBAC[F, Role, User, JwtToken]
  type SecuredHandler[F[_]] = SecuredRequestHandler[F, String, User, JwtToken]

  given authRole[F[_]: Applicative]: AuthorizationInfo[F, Role, User] with {
    override def fetchInfo(u: User): F[Role] = u.role.pure[F]
  }

  def allRoles[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC.all[F, Role, User, JwtToken]

  def recruiterOnly[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC(Role.RECRUITER)

  def adminOnly[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC(Role.ADMIN)

  case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoute[F]]])
  object Authorizations {
    given combiner[F[_]]: Semigroup[Authorizations[F]] = Semigroup.instance { (authA, authB) =>
      Authorizations(authA.rbacRoutes |+| authB.rbacRoutes)
    }
  }

  extension [F[_]](authRoute: AuthRoute[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(authRoute)))

  given authToTsec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    authz => {
      val unauthorizedService: TSecAuthService[User, JwtToken, F] =
        TSecAuthService[User, JwtToken, F] { _ =>
          Response[F](Status.Unauthorized).pure[F]
        }

      authz.rbacRoutes.toSeq.foldLeft(unauthorizedService) { case (acc, (rbac, routes)) =>
        val bigRoute = routes.reduce(_.orElse(_))
        TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, acc.run)
      }
    }
}
