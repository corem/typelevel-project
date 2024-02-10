package com.corem.jobsboard.pages

import scala.util.Try

import cats.effect.IO
import cats.syntax.traverse.*

import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import tyrian.Cmd
import tyrian.Html.*
import tyrian.Html
import tyrian.http.*
import tyrian.cmds.Logger

import com.corem.jobsboard.*
import com.corem.jobsboard.core.*
import com.corem.jobsboard.common.*
import com.corem.jobsboard.domain.job.*
import org.scalajs.dom.File
import org.scalajs.dom.FileReader

case class PostJobPage(
    company: String = "",
    title: String = "",
    description: String = "",
    externalUrl: String = "",
    remote: Boolean = false,
    location: String = "",
    salaryLo: Option[Int] = None,
    salaryHi: Option[Int] = None,
    currency: Option[String] = None,
    country: Option[String] = None,
    tags: Option[String] = None,
    image: Option[String] = None,
    seniority: Option[String] = None,
    other: Option[String] = None,
    status: Option[Page.Status] = None
) extends FormPage("Post Job", status) {
  import PostJobPage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateCompany(v)     => (this.copy(company = v), Cmd.None)
    case UpdateTitle(v)       => (this.copy(title = v), Cmd.None)
    case UpdateDescription(v) => (this.copy(description = v), Cmd.None)
    case UpdateExternalUrl(v) => (this.copy(externalUrl = v), Cmd.None)
    case ToggleRemote         => (this.copy(remote = !this.remote), Cmd.None)
    case UpdateLocation(v)    => (this.copy(location = v), Cmd.None)
    case UpdateSalaryLo(v)    => (this.copy(salaryLo = Some(v)), Cmd.None)
    case UpdateSalaryHi(v)    => (this.copy(salaryHi = Some(v)), Cmd.None)
    case UpdateCurrency(v)    => (this.copy(currency = Some(v)), Cmd.None)
    case UpdateCountry(v)     => (this.copy(country = Some(v)), Cmd.None)
    case UpdateImageFile(maybeFile) =>
      (this, Commands.loadFile(maybeFile))
    case UpdateImage(maybeImage) =>
      (this.copy(image = maybeImage), Cmd.None)
    case UpdateTags(v)      => (this.copy(tags = Some(v)), Cmd.None)
    case UpdateSeniority(v) => (this.copy(seniority = Some(v)), Cmd.None)
    case UpdateOther(v)     => (this.copy(other = Some(v)), Cmd.None)
    case AttemptPostJob =>
      (
        this,
        Commands.postJob(promoted = true)(
          company,
          title,
          description,
          externalUrl,
          remote,
          location,
          salaryLo,
          salaryHi,
          currency,
          country,
          tags,
          image,
          seniority,
          other
        )
      )
    case PostJobError(error) => (setErrorStatus(error), Cmd.None)
    case PostJobSuccess(jobId) =>
      (setSuccessStatus("Success!"), Logger.consoleLog[IO](s"Posted job with id $jobId"))
    case _ => (this, Cmd.None)
  }

  override protected def renderFormContent(): List[Html[App.Msg]] =
    if (!Session.isActive) renderInvalidContents()
    else
      List(
        renderInput("Company", "company", "text", true, UpdateCompany(_)),
        renderInput("Title", "title", "text", true, UpdateTitle(_)),
        renderTextArea("Description", "description", true, UpdateDescription(_)),
        renderInput("ExternalUrl", "externalurl", "text", true, UpdateExternalUrl(_)),
        renderToggle("Remote", "remote", true, _ => ToggleRemote),
        renderInput("Location", "location", "text", true, UpdateLocation(_)),
        renderInput(
          "salaryLo",
          "salaryLo",
          "number",
          false,
          s => UpdateSalaryLo(parseNumber(s))
        ),
        renderInput(
          "salaryHi",
          "salaryHi",
          "number",
          false,
          s => UpdateSalaryHi(parseNumber(s))
        ),
        renderInput("Currency", "currency", "text", false, UpdateCurrency(_)),
        renderInput("Country", "country", "text", false, UpdateCountry(_)),
        renderImageUploadInput("Logo", "logo", image, UpdateImageFile(_)),
        renderInput("Tags", "tags", "text", false, UpdateTags(_)),
        renderInput("Seniority", "seniority", "text", false, UpdateSeniority(_)),
        renderInput("Other", "other", "text", false, UpdateOther(_)),
        button(`type` := "button", onClick(AttemptPostJob))("Post Job")
      )

  private def renderInvalidContents() = List(
    p(`class` := "form-text")("You need to be logged in to post a job.")
  )

  private def parseNumber(s: String) =
    Try(s.toInt).getOrElse(0)

  private def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object PostJobPage {
  trait Msg                                           extends App.Msg
  case class UpdateCompany(company: String)           extends Msg
  case class UpdateTitle(title: String)               extends Msg
  case class UpdateDescription(description: String)   extends Msg
  case class UpdateExternalUrl(externalUrl: String)   extends Msg
  case object ToggleRemote                            extends Msg
  case class UpdateLocation(location: String)         extends Msg
  case class UpdateSalaryLo(salaryLo: Int)            extends Msg
  case class UpdateSalaryHi(salaryHi: Int)            extends Msg
  case class UpdateCurrency(currency: String)         extends Msg
  case class UpdateCountry(country: String)           extends Msg
  case class UpdateImageFile(maybeFile: Option[File]) extends Msg
  case class UpdateImage(maybeImage: Option[String])  extends Msg
  case class UpdateTags(tags: String)                 extends Msg
  case class UpdateSeniority(seniority: String)       extends Msg
  case class UpdateOther(other: String)               extends Msg
  case object AttemptPostJob                          extends Msg
  case class PostJobError(error: String)              extends Msg
  case class PostJobSuccess(jobId: String)            extends Msg

  object Endpoints {
    val postJob = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.postJob
      override val method: Method            = Method.Post
      override val onError: HttpError => Msg = e => PostJobError(e.toString)
      override val onResponse: Response => Msg =
        Endpoint.onResponseText(PostJobSuccess(_), PostJobError(_))
    }

    val postJobPromoted = new Endpoint[App.Msg] {
      override val location: String              = Constants.endpoints.postJobPromoted
      override val method: Method                = Method.Post
      override val onError: HttpError => App.Msg = e => PostJobError(e.toString)
      override val onResponse: Response => App.Msg =
        Endpoint.onResponseText(Router.ExternalRedirect(_), PostJobError(_))
    }
  }

  object Commands {
    def postJob(promoted: Boolean = true)(
        company: String,
        title: String,
        description: String,
        externalUrl: String,
        remote: Boolean,
        location: String,
        salaryLo: Option[Int],
        salaryHi: Option[Int],
        currency: Option[String],
        country: Option[String],
        tags: Option[String],
        image: Option[String],
        seniority: Option[String],
        other: Option[String]
    ) = {
      val endpoint =
        if (promoted) Endpoints.postJobPromoted
        else Endpoints.postJob

      endpoint.callAuthorized(
        JobInfo(
          company,
          title,
          description,
          externalUrl,
          remote,
          location,
          salaryLo,
          salaryHi,
          currency,
          country,
          tags.map(text => text.split(",").map(_.trim).toList),
          image,
          seniority,
          other
        )
      )
    }

    def loadFile(maybeFile: Option[File]) =
      Cmd.Run[IO, Option[String], Msg](
        maybeFile.traverse { file =>
          IO.async_ { cb =>
            val reader = new FileReader
            reader.onload = _ => cb(Right(reader.result.toString))
            reader.readAsDataURL(file)
          }
        }
      )(UpdateImage(_))
  }
}
