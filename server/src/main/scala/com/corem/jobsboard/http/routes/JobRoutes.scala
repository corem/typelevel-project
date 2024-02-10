package com.corem.jobsboard.http.routes

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*

import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import cats.effect.*
import cats.implicits.*
import cats.MonadThrow
import org.typelevel.log4cats.Logger
import tsec.authentication.asAuthed
import tsec.authentication.SecuredRequestHandler
import scala.language.implicitConversions
import org.typelevel.ci.CIStringSyntax

import java.util.UUID
import scala.collection.mutable

import com.corem.jobsboard.core.*
import com.corem.jobsboard.domain.job.*
import com.corem.jobsboard.domain.pagination.*
import com.corem.jobsboard.domain.security.*
import com.corem.jobsboard.http.responses.*
import com.corem.jobsboard.logging.syntax.*
import com.corem.jobsboard.http.validation.syntax.*
import com.corem.jobsboard.domain.user.*

class JobRoutes[F[_]: Concurrent: Logger: SecuredHandler] private (jobs: Jobs[F], stripe: Stripe[F])
    extends HttpValidationDsl[F] {

  object SkipQueryParem  extends OptionalQueryParamDecoderMatcher[Int]("skip")
  object LimitQueryParem extends OptionalQueryParamDecoderMatcher[Int]("limit")

  private val allFiltersRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "filters" =>
    jobs.possibleFilters().flatMap(jf => Ok(jf))
  }

  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root :? LimitQueryParem(limit) +& SkipQueryParem(skip) =>
      for {
        filter   <- req.as[JobFilter]
        jobsList <- jobs.all(filter, Pagination(limit, skip))
        resp     <- Ok(jobsList)
      } yield resp
  }

  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / UUIDVar(id) =>
    jobs.find(id).flatMap {
      case Some(job) => Ok(job)
      case None      => NotFound(FailureResponse(s"Job $id not found"))
    }
  }

  private val createJobRoute: AuthRoute[F] = { case req @ POST -> Root / "create" asAuthed user =>
    req.request.validate[JobInfo] { jobInfo =>
      for {
        jobId <- jobs.create(user.email, jobInfo)
        resp  <- Created(jobId)
      } yield resp
    }
  }

  private val updateJobRoute: AuthRoute[F] = { case req @ PUT -> Root / UUIDVar(id) asAuthed user =>
    req.request.validate[JobInfo] { jobInfo =>
      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Cannot update job $id: not found"))
        case Some(job) if user.owns(job) || user.isAdmin => jobs.update(id, jobInfo) *> Ok()
        case _ => Forbidden(FailureResponse("You can only delete your own jobs"))
      }
    }
  }

  private val deleteJobRoute: AuthRoute[F] = {
    case req @ DELETE -> Root / UUIDVar(id) asAuthed user =>
      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Cannot delete job $id: not found"))
        case Some(job) if user.owns(job) || user.isAdmin =>
          jobs.delete(id) *> Ok()
        case _ => Forbidden(FailureResponse("You can only delete your own jobs"))
      }
  }

  private val promotedJobRoute: AuthRoute[F] = {
    case req @ POST -> Root / "promoted" asAuthed user =>
      req.request.validate[JobInfo] { jobInfo =>
        for {
          jobId   <- jobs.create(user.email, jobInfo)
          session <- stripe.createCheckoutSession(jobId.toString, user.email)
          resp    <- session.map(se => Ok(se.getUrl())).getOrElse(NotFound())
        } yield resp
      }
  }

  private val promotedJobWebhook: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "webhook" =>
      val stripeSigHeader =
        req.headers.get(ci"Stripe-Signature").flatMap(_.toList.headOption).map(_.value)
      stripeSigHeader match {
        case Some(signature) =>
          for {
            payload <- req.bodyText.compile.string
            handled <- stripe.handleWebhookEvent(
              payload,
              signature,
              jobId => jobs.activate(UUID.fromString(jobId))
            )
            resp <- if (handled.nonEmpty) Ok() else NoContent()
          } yield resp
        case None =>
          Logger[F].info("Got webhook event with no Stripe signature") *>
            Forbidden("No Stripe signature")
      }
  }

  val unauthedRoutes = (allFiltersRoute <+> allJobsRoute <+> findJobRoute <+> promotedJobWebhook)
  val authedRoutes = SecuredHandler[F].liftService(
    createJobRoute.restrictedTo(allRoles) |+|
      promotedJobRoute.restrictedTo(allRoles) |+|
      updateJobRoute.restrictedTo(allRoles) |+|
      deleteJobRoute.restrictedTo(allRoles)
  )

  val routes = Router(
    "/jobs" -> (unauthedRoutes <+> authedRoutes)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger: SecuredHandler](jobs: Jobs[F], stripe: Stripe[F]) =
    new JobRoutes[F](jobs, stripe)
}
