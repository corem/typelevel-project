package com.corem.jobsboard.components

import tyrian.*
import tyrian.Html.*

import com.corem.jobsboard.*
import com.corem.jobsboard.domain.job.*
import com.corem.jobsboard.pages.*
import com.corem.jobsboard.common.Constants

object JobComponents {

  def card(job: Job): Html[App.Msg] =
    div(`class` := "jvm-recent-jobs-cards")(
      div(`class` := "jvm-recent-jobs-card-img")(
        img(
          `class` := "img-fluid",
          src     := job.jobInfo.image.getOrElse(Constants.jobImagePlaceholder),
          alt     := job.jobInfo.title
        )
      ),
      div(`class` := "jvm-recent-jobs-card-contents")(
        h4(
          Anchors.renderSimpleNavLink(
            s"${job.jobInfo.company} - ${job.jobInfo.title}",
            Page.Urls.JOB(job.id.toString()),
            "job-title-link"
          )
        ),
        renderJobSummary(job)
      ),
      div(`class` := "jvm-recent-jobs-card-btn-apply")(
        a(href := job.jobInfo.externalUrl, target := "blank")(
          button(`type` := "button", `class` := "btn btn-danger")("Apply")
        )
      )
    )

  def renderJobSummary(job: Job): Html[App.Msg] =
    div(`class` := "job-summary")(
      renderDetail("dollar", fullSalaryString(job)),
      renderDetail("location-dot", fullLocationString(job)),
      maybeRenderDetail("ranking-star", job.jobInfo.seniority),
      maybeRenderDetail("tags", job.jobInfo.tags.map(_.mkString(", ")))
    )

  def maybeRenderDetail(icon: String, maybeValue: Option[String]): Html[App.Msg] =
    maybeValue.map(value => renderDetail(icon, value)).getOrElse(div())

  def renderDetail(icon: String, value: String): Html[App.Msg] =
    div(`class` := "job-detail")(
      i(`class` := s"fa fa-$icon job-detail-icon")(),
      p(`class` := "job-detail-value")(value)
    )

  private def fullSalaryString(job: Job) = {
    val currency = job.jobInfo.currency.getOrElse("")
    (job.jobInfo.salaryLo, job.jobInfo.salaryHi) match {
      case (Some(lo), Some(hi)) => s"$currency $lo-$hi"
      case (Some(lo), None)     => s"> $currency $lo"
      case (None, Some(hi))     => s"up to $currency $hi"
      case _                    => "Unspecified salary"
    }
  }

  private def fullLocationString(job: Job) = job.jobInfo.country match {
    case Some(country) => s"${job.jobInfo.location}, $country"
    case None          => job.jobInfo.location
  }
}
