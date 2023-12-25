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

class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F], authenticator: Authenticator[F])
    extends HttpValidationDsl[F] {

  private val securedHandler: SecuredHandler[F] =
    SecuredRequestHandler(authenticator)

  object SkipQueryParem  extends OptionalQueryParamDecoderMatcher[Int]("skip")
  object LimitQueryParem extends OptionalQueryParamDecoderMatcher[Int]("limit")

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

  private def createJob(jobInfo: JobInfo): F[Job] = {
    Job(
      id = UUID.randomUUID(),
      date = System.currentTimeMillis(),
      ownerEmail = "Todo",
      jobInfo = jobInfo,
      active = true
    ).pure[F]
  }

  private val createJobRoute: AuthRoute[F] = { case req @ POST -> Root / "create" asAuthed _ =>
    req.request.validate[JobInfo] { jobInfo =>
      for {
        jobId <- jobs.create("cornet.remi@corem.corp", jobInfo)
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

  val unauthedRoutes = (allJobsRoute <+> findJobRoute)
  val authedRoutes = securedHandler.liftService(
    createJobRoute.restrictedTo(allRoles) |+|
      updateJobRoute.restrictedTo(allRoles) |+|
      deleteJobRoute.restrictedTo(allRoles)
  )

  val routes = Router(
    "/jobs" -> (unauthedRoutes <+> authedRoutes)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F], authenticator: Authenticator[F]) =
    new JobRoutes[F](jobs, authenticator)
}
