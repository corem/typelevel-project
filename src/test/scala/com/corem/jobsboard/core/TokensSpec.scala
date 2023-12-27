package com.corem.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*

import com.corem.jobsboard.fixtures.*
import com.corem.jobsboard.config.TokenConfig

class TokensSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DoobieSpec
    with UserFixture {
  val initScript: String   = "sql/recoverytokens.sql"
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Tokens algebra" - {
    "should not create token for a non-existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken("somebody@someemail.com")
        } yield token

        program.asserting(_ shouldBe None)
      }
    }

    "should create a token for a existing user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken(remiEmail)
        } yield token

        program.asserting(_ shouldBe defined)
      }
    }

    "should not validate expired tokens" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100L))
          maybeToken <- tokens.getToken(remiEmail)
          _          <- IO.sleep(500.millis)
          isTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(remiEmail, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid

        program.asserting(_ shouldBe false)
      }
    }

    "should validate fresh tokens" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          maybeToken <- tokens.getToken(remiEmail)
          isTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(remiEmail, token)
            case None        => IO.pure(false)
          }
        } yield isTokenValid

        program.asserting(_ shouldBe true)
      }
    }

    "should only validate tokens for the user that generated them" in {
      transactor.use { xa =>
        val program = for {
          tokens     <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          maybeToken <- tokens.getToken(remiEmail)
          isRemiTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken(remiEmail, token)
            case None        => IO.pure(false)
          }
          isOtherTokenValid <- maybeToken match {
            case Some(token) => tokens.checkToken("someoneelse@gmail.com", token)
            case None        => IO.pure(false)
          }
        } yield (isRemiTokenValid, isOtherTokenValid)

        program.asserting { case (isRemiTokenValid, isOtherTokenValid) =>
          isRemiTokenValid shouldBe true
          isOtherTokenValid shouldBe false
        }
      }
    }
  }
}
