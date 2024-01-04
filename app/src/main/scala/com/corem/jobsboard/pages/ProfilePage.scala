package com.corem.jobsboard.pages

import cats.effect.IO

import tyrian.*
import tyrian.Html.*
import tyrian.http.*

import io.circe.generic.auto.*

import com.corem.jobsboard.*
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.common.*
import com.corem.jobsboard.core.*

final case class ProfilePage(
    oldPassword: String = "",
    newPassword: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Profile", status) {
  import ProfilePage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateOldPassword(p)       => (this.copy(oldPassword = p), Cmd.None)
    case UpdateNewPassword(p)       => (this.copy(newPassword = p), Cmd.None)
    case AttemptChangePassword      => (this, Commands.changePassword(oldPassword, newPassword))
    case ChangePasswordError(error) => (setErrorStatus(error), Cmd.None)
    case ChangePasswordSuccess      => (setSuccessStatus("Succes !"), Cmd.None)
    case _                          => (this, Cmd.None)
  }

  override def view(): Html[App.Msg] =
    if (Session.isActive) super.view()
    else renderInvalidPage

  override protected def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Old Password", "oldPassword", "password", true, UpdateOldPassword(_)),
    renderInput("New Password", "newPassword", "password", true, UpdateNewPassword(_)),
    button(`type` := "button", onClick(AttemptChangePassword))("Change Password")
  )

  private def renderInvalidPage =
    div(
        h1("Ouch!"),
        div("It seems you are not logged in yet.")
    )

  private def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object ProfilePage {
  trait Msg                                      extends App.Msg
  case class UpdateOldPassword(password: String) extends Msg
  case class UpdateNewPassword(password: String) extends Msg
  case object AttemptChangePassword              extends Msg
  case class ChangePasswordError(error: String)  extends Msg
  case object ChangePasswordSuccess              extends Msg

  object Endpoints {
    val changePassword = new Endpoint[Msg] {
      override val location: String          = Constants.endpoints.changePassword
      override val method: Method            = Method.Put
      override val onError: HttpError => Msg = e => ChangePasswordError(e.toString)
      override val onResponse: Response => Msg = _.status match {
        case Status(200, _) => ChangePasswordSuccess
        case Status(404, _) => ChangePasswordError("Server says this user does not exists.")
        case Status(s, _) if s >= 400 && s < 500 => ChangePasswordError("Invalid credentials")
        case _                                   => ChangePasswordError("Unkown reply from server.")
      }
    }
  }

  object Commands {
    def changePassword(oldPassword: String, newPassword: String) =
      Endpoints.changePassword.callAuthorized(NewPasswordInfo(oldPassword, newPassword))
  }
}
