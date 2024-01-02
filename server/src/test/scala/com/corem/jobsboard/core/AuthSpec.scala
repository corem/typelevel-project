package com.corem.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.mac.jca.HMACSHA256
import tsec.authentication.JWTAuthenticator
import tsec.authentication.IdentityStore
import scala.concurrent.duration.*
import cats.data.OptionT
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

import com.corem.jobsboard.domain.user.*
import com.corem.jobsboard.config.*
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.domain.security.*
import com.corem.jobsboard.fixtures.*

class AuthSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with UserFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val mockedConfig = SecurityConfig("secret", 1.day)

  val mockedTokens: Tokens[IO] = new Tokens[IO] {
    override def getToken(email: String): IO[Option[String]] =
      if (email == remiEmail) IO.pure(Some("abc123"))
      else IO.pure(None)
    override def checkToken(email: String, token: String): IO[Boolean] =
      IO.pure(token == "abc123")
  }

  val mockedEmails: Emails[IO] = new Emails[IO] {
    override def sendEmail(to: String, subject: String, content: String): IO[Unit] = IO.unit
    override def sendPasswordRecoveryEmail(to: String, token: String): IO[Unit]    = IO.unit
  }

  def probedEmail(users: Ref[IO, Set[String]]): Emails[IO] = new Emails[IO] {
    override def sendEmail(to: String, subject: String, content: String): IO[Unit] =
      users.modify(set => (set + to, ()))
    override def sendPasswordRecoveryEmail(to: String, token: String): IO[Unit] =
      sendEmail(to, "your token", "token")
  }

  "Auth algebra" - {
    "login should return None if the user does not exist" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login("notfound@corem.corp", "password")
      } yield maybeToken

      program.asserting(_ shouldBe None)
    }

    "login should return None if the user exists but the password is wrong" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login(remiEmail, "wrongpassword")
      } yield maybeToken

      program.asserting(_ shouldBe None)
    }

    "login should return a token if the user exists and the password is correct" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        maybeToken <- auth.login(
          remiEmail,
          "$2a$10$jY60jL/9Lv6./UHhhj2ZvOSm8PQIiTueC4gmsegrD5K.Yi6/mGY.m"
        )
      } yield maybeToken

      program.asserting(_ shouldBe None)
    }

    "signing up should not create a user with an existing email" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        mayberUser <- auth.signUp(
          NewUserInfo(remiEmail, "somePassword", Some("Remi"), Some("Cornet"), Some("Company"))
        )
      } yield mayberUser

      program.asserting(_ shouldBe None)
    }

    "signing up should create a completely new user" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        mayberUser <- auth.signUp(
          NewUserInfo(
            "newuser@corem.corp",
            "somePassword",
            Some("New"),
            Some("User"),
            Some("Company")
          )
        )
      } yield mayberUser

      program.asserting {
        case Some(user) =>
          user.email shouldBe "newuser@corem.corp"
          user.firstName shouldBe Some("New")
          user.lastName shouldBe Some("User")
          user.company shouldBe Some("Company")
          user.role shouldBe Role.RECRUITER
        case _ => fail()
      }
    }

    "changePassword should return Right(None) if the user does not exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword(
          "notfound@corem.corp",
          NewPasswordInfo("oldPassword", "newPassword")
        )
      } yield result

      program.asserting(_ shouldBe Right(None))
    }

    "changePassword should return Left(Error) if the password is incorrect" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword(
          remiEmail,
          NewPasswordInfo("wrongPassword", "newPassword")
        )
      } yield result

      program.asserting(_ shouldBe Left("Invalid password"))
    }

    "changePassword should correctly change password if all details are correct" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.changePassword(
          remiEmail,
          NewPasswordInfo(
            "remimypassword",
            "remimynewpassword"
          )
        )
        isNicePassword <- result match {
          case Right(Some(user)) =>
            BCrypt
              .checkpwBool[IO](
                "remimynewpassword",
                PasswordHash[BCrypt]("$2a$10$FCIk9SHgFy2jqkIpkxMMZOAIRJEcarctbSWDT.zAxy0bKCwbHkosu")
              )
          case _ => IO.pure(false)
        }
      } yield isNicePassword

      program.asserting(_ shouldBe true)
    }

    "recoverPassword should fail for a user that does not exist, even if the token is correct" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result1 <- auth.recoverPasswordFromToken(
          "someone@gmail.com",
          "abc123",
          "igotya"
        )
        result2 <- auth.recoverPasswordFromToken(
          "someone@gmail.com",
          "wrongtoken",
          "igotya"
        )
      } yield (result1, result2)

      program.asserting(_ shouldBe (false, false))
    }

    "recoverPassword should fail for a user that exist but provides an incorrect token" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.recoverPasswordFromToken(
          remiEmail,
          "wrongToken",
          "h4ck3d"
        )
      } yield result

      program.asserting(_ shouldBe false)
    }

    "recoverPassword should succeed for a correct combination of user/token" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)
        result <- auth.recoverPasswordFromToken(
          remiEmail,
          "abc123",
          "h4ck3d"
        )
      } yield result

      program.asserting(_ shouldBe true)
    }

    "sendingRecoveryPassword should fail for a user that does not exist" in {
      val program = for {
        set    <- Ref.of[IO, Set[String]](Set())
        emails <- IO(probedEmail(set))
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result <- auth.sendPasswordRecoveryToken(
          "someone@whatever.com"
        )
        userBeingSentEmails <- set.get
      } yield userBeingSentEmails

      program.asserting(_ shouldBe empty)
    }

    "sendingRecoveryPassword should succeed for a user that exists" in {
      val program = for {
        set    <- Ref.of[IO, Set[String]](Set())
        emails <- IO(probedEmail(set))
        auth   <- LiveAuth[IO](mockedUsers, mockedTokens, emails)
        result <- auth.sendPasswordRecoveryToken(
          remiEmail
        )
        userBeingSentEmails <- set.get
      } yield userBeingSentEmails

      program.asserting(_ should contain(remiEmail))
    }
  }
}
