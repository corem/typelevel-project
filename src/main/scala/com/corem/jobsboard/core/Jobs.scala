package com.corem.jobsboard.core

import cats.*
import cats.implicits.*
import cats.effect.*

import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*

import java.util.UUID
import com.corem.jobsboard.domain.job.Job
import com.corem.jobsboard.domain.job.JobInfo

trait Jobs[F[_]] {
  def create(ownerEmail: String, jobInfo: JobInfo): F[UUID]
  def all(): F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(id: UUID, jobInfo: JobInfo): F[Option[Job]]
  def delete(id: UUID): F[Int]
}

// id: UUID,
// date: Long,
// ownerEmail: String,
// jobInfo: JobInfo,
// active: Boolean = false
// company: String,
// title: String,
// description: String,
// externalUrl: String,
// remote: Boolean,""
// location: String,
// salaryLo: Option[Int],
// salaryHi: Option[Int],
// currency: Option[String],
// country: Option[String],
// tags: Option[List[String]],
// image: Option[String],
// seniority: Option[String],
// other: Option[String]

class LiveJobs[F[_]: MonadCancelThrow] private (xa: Transactor[F]) extends Jobs[F] {
  override def create(ownerEmail: String, jobInfo: JobInfo): F[UUID] = ???

  override def all(): F[List[Job]] =
    sql"""
        SELECT
            id
            date
            ownerEmail
            jobInfo
            active
            company
            title
            description
            externalUrl
            remote
            location
            salaryLo
            salaryHi
            currency
            country
            tags
            image
            seniority
            other
        FROM jobs
        """
      .query[Job]
      .to[List]
      .transact(xa)

  override def find(id: UUID): F[Option[Job]] = ???

  override def update(id: UUID, jobInfo: JobInfo): F[Option[Job]] = ???

  override def delete(id: UUID): F[Int] = ???
}

object LiveJobs {
  given jobRead: Read[Job] = Read[
    (
        UUID,
        Long,
        String,
        String,
        String,
        String,
        String,
        Boolean,
        String,
        Option[Int],
        Option[Int],
        Option[String],
        Option[String],
        Option[List[String]],
        Option[String],
        Option[String],
        Option[String],
        Boolean
    )
  ].map {
    case (
          id: UUID,
          date: Long,
          ownerEmail: String,
          company: String,
          title: String,
          description: String,
          externalUrl: String,
          remote: Boolean,
          location: String,
          salaryLo: Option[Int] @unchecked,
          salaryHi: Option[Int] @unchecked,
          currency: Option[String] @unchecked,
          country: Option[String] @unchecked,
          tags: Option[List[String]] @unchecked,
          image: Option[String] @unchecked,
          seniority: Option[String] @unchecked,
          other: Option[String] @unchecked,
          active: Boolean
        ) =>
      Job(
        id = id,
        date = date,
        ownerEmail = ownerEmail,
        JobInfo(
          company = company,
          title = title,
          description = description,
          externalUrl = externalUrl,
          remote = remote,
          location = location,
          salaryLo = salaryLo,
          salaryHi = salaryHi,
          currency = currency,
          country = country,
          tags = tags,
          image = image,
          seniority = seniority,
          other = other
        ),
        active = active
      )
  }

  def apply[F[_]: MonadCancelThrow](xa: Transactor[F]): F[LiveJobs[F]] = new LiveJobs[F](xa).pure[F]
}
