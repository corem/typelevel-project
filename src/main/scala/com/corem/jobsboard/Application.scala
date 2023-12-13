package com.corem.jobsboard

import cats.*
import cats.effect.*
import cats.effect.IO
import cats.effect.IOApp

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
import com.corem.jobsboard.http.routes.HealthRoutes
import com.corem.jobsboard.config.syntax.loadF

object Application extends IOApp.Simple {

  val configSource = ConfigSource.default.load[EmberConfig]

  override def run = ConfigSource.default.loadF[IO, EmberConfig].flatMap { config =>
    EmberServerBuilder
      .default[IO]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(HealthRoutes[IO].routes.orNotFound)
      .build
      .use(_ => IO.println("Server ready!") *> IO.never)
  }
}
