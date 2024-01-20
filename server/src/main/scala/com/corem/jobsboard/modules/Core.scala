package com.corem.jobsboard.modules

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import doobie.util.transactor.Transactor

import com.corem.jobsboard.core.*
import com.corem.jobsboard.config.*

final class Core[F[_]] private (
    val jobs: Jobs[F],
    val users: Users[F],
    val auth: Auth[F],
    val stripe: Stripe[F]
)

object Core {
  def apply[F[_]: Async: Logger](
      xa: Transactor[F],
      tokenConfig: TokenConfig,
      emailServiceConfig: EmailServiceConfig,
      stripeConfig: StripeConfig
  ): Resource[F, Core[F]] = {
    val coreF = for {
      jobs   <- LiveJobs[F](xa)
      users  <- LiveUsers[F](xa)
      tokens <- LiveTokens[F](users)(xa, tokenConfig)
      emails <- LiveEmails[F](emailServiceConfig)
      auth   <- LiveAuth[F](users, tokens, emails)
      stripe <- LiveStripe[F](stripeConfig)
    } yield new Core(jobs, users, auth, stripe)

    Resource.eval(coreF)
  }
}
