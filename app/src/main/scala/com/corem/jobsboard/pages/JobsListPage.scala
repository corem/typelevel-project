package com.corem.jobsboard.pages

import cats.effect.IO

import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import tyrian.Html
import tyrian.http.*
import tyrian.Cmd
import tyrian.Html.*
import tyrian.cmds.Logger

import com.corem.jobsboard.common.*
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.domain.job.*
import com.corem.jobsboard.common.Endpoint
import com.corem.jobsboard.*
import com.corem.jobsboard.core.*
import com.corem.jobsboard.components.*
import com.corem.jobsboard.pages.JobListPage.FilterJobs

final case class JobsListPage(
    filterPanel: FilterPanel = FilterPanel(
      filterAction = FilterJobs(_)
    ),
    jobFilter: JobFilter = JobFilter(),
    jobs: List[Job] = List(),
    canLoadMore: Boolean = true,
    status: Option[Page.Status] = Some(Page.Status.LOADING)
) extends Page {
  import JobListPage.*

  override def initCmd: Cmd[IO, App.Msg] =
    filterPanel.initCmd |+| Commands.getJobs()

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case AddJobs(list, clm) =>
      (
        setSuccessStatus("Loaded").copy(jobs = this.jobs ++ list, canLoadMore = clm),
        Cmd.None
      )
    case SetErrorStatus(e) => (setErrorStatus(e), Cmd.None)
    case LoadMoreJobs      => (this, Commands.getJobs(filter = jobFilter, skip = jobs.length))
    case FilterJobs(selectedFilters) =>
      val newJobFilter = createJobFilter(selectedFilters)
      (this.copy(jobs = List(), jobFilter = newJobFilter), Commands.getJobs(filter = newJobFilter))
    case msg: FilterPanel.Msg =>
      val (newFilterPanel, cmd) = filterPanel.update(msg)
      (this.copy(filterPanel = newFilterPanel), cmd)
    case _ => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    section(`class` := "section-1")(
      div(`class` := "container job-list-hero")(
        h1(`class` := "job-list-title")("Jobs Board")
      ),
      div(`class` := "container")(
        div(`class` := "row jvm-recent-jobs-body")(
          div(`class` := "col-lg-4")(
            filterPanel.view()
          ),
          div(`class` := "col-lg-8")(
            jobs.map(renderJob) ++ maybeRenderLoadMore
          )
        )
      )
    )

  private def renderJob(job: Job) =
    JobComponents.card(job)

  private def renderJobSummary(job: Job): Html[App.Msg] =
    div(
      JobComponents.renderDetail("location-dot", job.jobInfo.location)
    )

  private def maybeRenderLoadMore: Option[Html[App.Msg]] = status.map { s =>
    div(`class` := "load-more-action")(
      s match {
        case Page.Status(_, Page.StatusKind.LOADING) => div(`class` := "page-status-loading")("Loading...")
        case Page.Status(e, Page.StatusKind.ERROR)   => div(`class` := "page-status-errors")(e) 
        case Page.Status(_, Page.StatusKind.SUCCESS) =>
          if (canLoadMore)
            button(`type` := "button", `class` := "load-more-btn", onClick(LoadMoreJobs))(
              "Load more"
            )
          else
            div("All jobs loaded")
      }
    )
  }

  private def createJobFilter(selectedFilters: Map[String, Set[String]]) =
    JobFilter(
      companies = selectedFilters.get("Companies").getOrElse(Set()).toList,
      locations = selectedFilters.get("Locations").getOrElse(Set()).toList,
      countries = selectedFilters.get("Countries").getOrElse(Set()).toList,
      seniorities = selectedFilters.get("Seniorities").getOrElse(Set()).toList,
      tags = selectedFilters.get("Tags").getOrElse(Set()).toList,
      maxSalary = Some(filterPanel.maxSalary).filter(_ > 0),
      filterPanel.remote
    )

  private def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object JobListPage {
  trait Msg                                                        extends App.Msg
  case class SetErrorStatus(e: String)                             extends Msg
  case class AddJobs(list: List[Job], canLoadMore: Boolean)        extends Msg
  case object LoadMoreJobs                                         extends Msg
  case class FilterJobs(selectedFilters: Map[String, Set[String]]) extends Msg

  object Endpoints {
    def getJobs(limit: Int = Constants.defaultPageSize, skip: Int = 0) = new Endpoint[Msg] {
      override val location: String = Constants.endpoints.jobs + s"?limit=$limit&skip=$skip"
      override val method: Method   = Method.Post
      override val onError: HttpError => Msg = e => SetErrorStatus(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponse[List[Job], Msg](
          list => AddJobs(list, canLoadMore = skip == 0 || !list.isEmpty),
          SetErrorStatus(_)
        )
    }
  }

  object Commands {
    def getJobs(
        filter: JobFilter = JobFilter(),
        limit: Int = Constants.defaultPageSize,
        skip: Int = 0
    ): Cmd[IO, Msg] =
      Endpoints.getJobs(limit, skip).call(filter)
  }
}
