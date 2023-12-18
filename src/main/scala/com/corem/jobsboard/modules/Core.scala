package com.corem.jobsboard.modules

import cats.effect.*
import cats.implicits.*

import com.corem.jobsboard.core.*
import doobie.util.transactor.Transactor

final class Core[F[_]] private (val jobs: Jobs[F])

object Core {
  def apply[F[_]: Async](xa: Transactor[F]): Resource[F, Core[F]] = {
    Resource
      .eval(LiveJobs[F](xa))
      .map(jobs => new Core(jobs))
  }
}
