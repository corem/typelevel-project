package com.corem.jobsboard.modules

import cats.effect.*
import doobie.util.*
import doobie.hikari.HikariTransactor

import com.corem.jobsboard.config.*

object Database {
    def makePostgresResource[F[_]: Async](config: PostgresConfig): Resource[F, HikariTransactor[F]] =
        for {
            ec <- ExecutionContexts.fixedThreadPool(config.nThreads)
            xa <- HikariTransactor.newHikariTransactor[F](
                "org.postgresql.Driver",
                config.url,
                config.user,
                config.pass,
                ec
            )
        } yield xa
}
