package com.corem.jobsboard.modules

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import doobie.util.transactor.Transactor

import com.corem.jobsboard.core.*

final class Core[F[_]] private (val jobs: Jobs[F])

object Core {
  def apply[F[_]: Async: Logger](xa: Transactor[F]): Resource[F, Core[F]] = {
    Resource
      .eval(LiveJobs[F](xa))
      .map(jobs => new Core(jobs))
  }
}
