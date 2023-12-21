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

import java.util.UUID
import scala.collection.mutable

import com.corem.jobsboard.core.*
import com.corem.jobsboard.domain.job.*
import com.corem.jobsboard.domain.pagination.*
import com.corem.jobsboard.http.responses.*
import com.corem.jobsboard.logging.syntax.*
import com.corem.jobsboard.http.validation.syntax.*

class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F]) extends HttpValidationDsl[F] {

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

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "create" =>
      req.validate[JobInfo] { jobInfo =>
        for {
          jobInfo <- req.as[JobInfo].logError(e => s"Parsing payload failed: $e")
          jobId   <- jobs.create("cornet.remi@corem.corp", jobInfo)
          resp    <- Created(jobId)
        } yield resp
      }
  }

  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      req.validate[JobInfo] { jobInfo =>
        for {
          maybeNewJob <- jobs.update(id, jobInfo)
          resp <- maybeNewJob match {
            case Some(job) => Ok()
            case None      => NotFound(FailureResponse(s"Cannot update job $id: not found"))
          }
        } yield resp
      }
  }

  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      jobs.find(id).flatMap {
        case Some(job) =>
          for {
            _    <- jobs.delete(id)
            resp <- Ok()
          } yield resp
        case None => NotFound(FailureResponse(s"Cannot delete job $id: not found"))
      }
  }

  val routes = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F]) = new JobRoutes[F](jobs)
}
