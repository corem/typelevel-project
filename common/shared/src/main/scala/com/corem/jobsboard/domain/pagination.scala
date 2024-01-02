package com.corem.jobsboard.domain

object pagination {
  final case class Pagination(limit: Int, skip: Int)

  object Pagination {
    val defaultPageSize = 20

    def apply(maybeLimit: Option[Int], maybeSkip: Option[Int]) = 
        new Pagination(maybeLimit.getOrElse(defaultPageSize), maybeSkip.getOrElse(0))

    def default =
      new Pagination(limit = defaultPageSize, skip = 0)
  }
}
