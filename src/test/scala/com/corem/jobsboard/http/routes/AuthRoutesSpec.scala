package com.corem.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.ci.CIString
import org.typelevel.ci.CIStringSyntax
import tsec.mac.jca.HMACSHA256
import tsec.authentication.JWTAuthenticator
import tsec.authentication.IdentityStore
import scala.concurrent.duration.*

import com.corem.jobsboard.core.*
import com.corem.jobsboard.domain.security.*
import com.corem.jobsboard.domain.user.*
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.fixtures.UserFixture
import cats.data.OptionT
import tsec.jws.mac.JWTMac
import org.http4s.headers.Authorization

class AuthRoutesSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with UserFixture {

  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == remiEmail) OptionT.pure(Remi)
      else if (email == gastonEmail) OptionT.pure(Gaston)
      else OptionT.none[IO, User]
    JWTAuthenticator.unbacked.inBearerToken(
      1.day,   // Token expiration
      None,    // Max idle time (optional)
      idStore, // Id Store
      key      // Hash key
    )
  }

  val mockedAuth: Auth[IO] = new Auth[IO] {
    def login(email: String, password: String): IO[Option[JwtToken]] =
      if (email == remiEmail && password == remiPassword)
        mockedAuthenticator.create(remiEmail).map(Some(_))
      else IO.pure(None)

    def signUp(newUserInfo: NewUserInfo): IO[Option[User]] =
      if (newUserInfo.email == gastonEmail)
        IO.pure(Some(Gaston))
      else
        IO.pure(None)

    def changePassword(
        email: String,
        newPasswordInfo: NewPasswordInfo
    ): IO[Either[String, Option[User]]] =
      if (email == remiEmail)
        if (newPasswordInfo.oldPassword == remiPassword)
          IO.pure(Right(Some(Remi)))
        else
          IO.pure(Left("Invalid password"))
      else
        IO.pure(Right(None))
      
    def delete(email: String): IO[Boolean] = IO.pure(true)

    def authenticator: Authenticator[IO] = mockedAuthenticator
  }

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] = {
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)
        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }
    }

  given logger: Logger[IO]       = Slf4jLogger.getLogger[IO]
  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth).routes

  "AuthRoutes" - {
    "should return a 401 - Unauthorized if login fails" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(remiEmail, "wrongpassword"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok + a JWT Token if login is successful" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(remiEmail, remiPassword))
        )
      } yield {
        response.status shouldBe Status.Ok
        response.headers.get(ci"Authorization") shouldBe defined
      }
    }

    "should return a 400 - Bad Request if the user to create already exists" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(NewUserRemi)
        )
      } yield {
        response.status shouldBe Status.BadRequest
      }
    }

    "should return a 201 - Created if the user was successfuly created" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/users")
            .withEntity(NewUserGaston)
        )
      } yield {
        response.status shouldBe Status.Created
      }
    }

    "should return a 401 - Unauthorized if login out without a valid JWT Token" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok if login out with a valid JWT Token" in {
      for {
        jwtToken <- mockedAuthenticator.create(remiEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
            .withBearerToken(jwtToken)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }

    "should return a 404 - Not Found if the user does not exist" in {
      for {
        jwtToken <- mockedAuthenticator.create(gastonEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo(gastonPassword, "newPassword"))
        )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }

    "should return a 403 - Forbidden if the old password is incorrect" in {
      for {
        jwtToken <- mockedAuthenticator.create(remiEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo("wrongPassword", "newPassword"))
        )
      } yield {
        response.status shouldBe Status.Forbidden
      }
    }

    "should return a 401 - Unauthorized if the JWT Token is invalid" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withEntity(NewPasswordInfo(remiPassword, "newPassword"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok if the password change succeed" in {
      for {
        jwtToken <- mockedAuthenticator.create(remiEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/users/password")
            .withBearerToken(jwtToken)
            .withEntity(NewPasswordInfo(remiPassword, "newPassword"))
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }

    "should return a 401 - Unauthorized if a non admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(gastonEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/auth/users/cornet.remi@corem.corp")
            .withBearerToken(jwtToken)
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }

    "should return a 200 - Ok if an admin tries to delete a user" in {
      for {
        jwtToken <- mockedAuthenticator.create(remiEmail)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"/auth/users/cornet.remi@corem.corp")
            .withBearerToken(jwtToken)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }
  }
}
