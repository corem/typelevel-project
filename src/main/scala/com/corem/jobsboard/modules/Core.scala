package com.corem.jobsboard.modules

import doobie.hikari.HikariTransactor
import cats.effect.*
import cats.implicits.*

import doobie.*
import doobie.implicits.*
import doobie.util.*

import com.corem.jobsboard.core.*

final class Core[F[_]] private (val jobs: Jobs[F])

object Core {
  def postgresResource[F[_]: Async]: Resource[F, HikariTransactor[F]] =
    for {
      ec <- ExecutionContexts.fixedThreadPool(32)
      xa <- HikariTransactor.newHikariTransactor[F](
        "org.postgresql.Driver",
        "jdbc:postgresql:board",
        "docker",
        "docker",
        ec
      )
    } yield xa

  def apply[F[_]: Async]: Resource[F, Core[F]] = {
    postgresResource[F]
      .evalMap(postgres => LiveJobs[F](postgres))
      .map(jobs => new Core(jobs))
  }

}
