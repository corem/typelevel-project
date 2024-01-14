package com.corem.jobsboard.pages

import cats.effect.IO

import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import tyrian.Html
import tyrian.http.Method
import tyrian.http.HttpError
import tyrian.http.Response
import tyrian.Cmd
import tyrian.Html.*
import tyrian.cmds.Logger

import com.corem.jobsboard.common.Constants
import com.corem.jobsboard.domain.auth.*
import com.corem.jobsboard.common.Endpoint
import com.corem.jobsboard.*
import com.corem.jobsboard.core.*
import com.corem.jobsboard.components.*

final case class LoginPage(
    email: String = "",
    password: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Log In", status) {
  import LoginPage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e)    => (this.copy(email = e), Cmd.None)
    case UpdatePassword(p) => (this.copy(password = p), Cmd.None)
    case AttemptLogin =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Please enter a password"), Cmd.None)
      else (this, Commands.login(LoginInfo(email, password)))
    case LoginError(error) =>
      (setErrorStatus(error), Cmd.None)
    case LoginSuccess(token) =>
      (setSuccessStatus("Success!"), Cmd.Emit(Session.SetToken(email, token, true)))
    case _ => (this, Cmd.None)
  }

  override def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", true, UpdateEmail(_)),
    renderInput("Password", "password", "password", true, UpdatePassword(_)),
    button(`type` := "button", onClick(AttemptLogin))("Log In"),
    Anchors.renderSimpleNavLink("Forgot password ?", Page.Urls.FORGOT_PASSWORD, "auth-link")
  )

  private def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  private def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))

}

object LoginPage {
  trait Msg                                   extends App.Msg
  case class UpdateEmail(email: String)       extends Msg
  case class UpdatePassword(password: String) extends Msg

  case object AttemptLogin extends Msg
  case object NoOp         extends Msg

  case class LoginError(error: String)   extends Msg
  case class LoginSuccess(token: String) extends Msg

  object Endpoints {
    val login = new Endpoint[Msg] {
      override val location: String = Constants.endpoints.login
      override val method: Method   = Method.Post
      override val onError: HttpError => Msg =
        e => LoginError(e.toString)
      override val onResponse: Response => Msg =
        response => {
          val maybeToken = response.headers.get("authorization")
          maybeToken match {
            case Some(token) => LoginSuccess(token)
            case None        => LoginError("Invalid username or password")
          }
        }
    }
  }

  object Commands {
    def login(loginInfo: LoginInfo) =
      Endpoints.login.call(loginInfo)
  }
}
