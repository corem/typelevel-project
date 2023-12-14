package com.corem.jobsboard

import cats.*
import cats.effect.*
import cats.effect.IO
import cats.effect.IOApp
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import org.http4s.*
import org.http4s.circe.*
import org.http4s.headers.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.ember.server.*
import org.http4s.server.*

import pureconfig.ConfigSource
import pureconfig.ConfigReader
import pureconfig.error.ConfigReaderException
import com.corem.jobsboard.config.*
import com.corem.jobsboard.config.syntax.*
import com.corem.jobsboard.http.HttpApi
import com.corem.jobsboard.config.syntax.loadF


object Application extends IOApp.Simple {
    
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run = ConfigSource.default.loadF[IO, EmberConfig].flatMap { config =>
    EmberServerBuilder
      .default[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(HttpApi[IO].endpoints.orNotFound)
      .build
      .use(_ => IO.println("Server ready!") *> IO.never)
  }
}
