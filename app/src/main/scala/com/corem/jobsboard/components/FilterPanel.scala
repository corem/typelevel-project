package com.corem.jobsboard.components

import cats.effect.IO

import io.circe.generic.auto.*

import tyrian.*
import tyrian.http.*
import tyrian.Html.*

import com.corem.jobsboard.*
import com.corem.jobsboard.common.*
import com.corem.jobsboard.App.Msg
import com.corem.jobsboard.domain.job.*

final case class FilterPanel(
    possibleFilters: JobFilter = JobFilter(),
    maybeError: Option[String] = None
) extends Component[App.Msg, FilterPanel] {
  import FilterPanel.*

  override def initCmd: Cmd[IO, App.Msg] =
    Commands.getFilters

  override def update(msg: App.Msg): (FilterPanel, Cmd[IO, App.Msg]) = msg match {
    case SetPossibleFitlers(possibleFilters) =>
      (this.copy(possibleFilters = possibleFilters), Cmd.None)
    case FilterPanelError(e) => (this.copy(maybeError = Some(e)), Cmd.None)
    case _                   => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    div(`class` := "filter-panel-container")(
      maybeRenderError(),
      div(possibleFilters.toString)
    )

  private def maybeRenderError() =
    maybeError
      .map { e =>
        div(`class` := "filter-panel-error")(e)
      }
      .getOrElse(div())
}

object FilterPanel {
  trait Msg                                                 extends App.Msg
  case class FilterPanelError(error: String)                extends Msg
  case class SetPossibleFitlers(possibleFitlers: JobFilter) extends Msg

  object Endpoints {
    val getFilters = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.getFilters
      override val method: Method            = Method.Get
      override val onError: HttpError => Msg = e => FilterPanelError(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponse[JobFilter, Msg](
          SetPossibleFitlers(_),
          FilterPanelError(_)
        )
    }
  }

  object Commands {
    def getFilters = Endpoints.getFilters.call()
  }
}
