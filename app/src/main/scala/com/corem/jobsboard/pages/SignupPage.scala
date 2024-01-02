package com.corem.jobsboard.pages

import cats.effect.IO

import io.circe.syntax.*
import io.circe.parser.*
import io.circe.generic.auto.*

import tyrian.Cmd
import tyrian.Html.*
import tyrian.Html
import tyrian.http.*
import tyrian.cmds.Logger

import com.corem.jobsboard.*
import com.corem.jobsboard.common.*
import com.corem.jobsboard.domain.auth.*

final case class SignupPage(
    email: String = "",
    password: String = "",
    confirmPassword: String = "",
    firstName: String = "",
    lastName: String = "",
    company: String = "",
    status: Option[Page.Status] = None
) extends FormPage("Sign Up", status) {
  import SignupPage.*

  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) = msg match {
    case UpdateEmail(e)           => (this.copy(email = e), Cmd.None)
    case UpdatePassword(p)        => (this.copy(password = p), Cmd.None)
    case UpdateConfirmPassword(c) => (this.copy(confirmPassword = c), Cmd.None)
    case UpdateFirstName(f)       => (this.copy(firstName = f), Cmd.None)
    case UpdateLastName(l)        => (this.copy(lastName = l), Cmd.None)
    case UpdateCompany(c)         => (this.copy(company = c), Cmd.None)
    case AttempSignUp =>
      if (!email.matches(Constants.emailRegex))
        (setErrorStatus("Email is invalid"), Cmd.None)
      else if (password.isEmpty)
        (setErrorStatus("Please enter a password"), Cmd.None)
      else if (password != confirmPassword)
        (setErrorStatus("Password fields do not match"), Cmd.None)
      else
        (
          this,
          Commands.signup(
            NewUserInfo(
              email,
              password,
              Option(firstName).filter(_.nonEmpty),
              Option(lastName).filter(_.nonEmpty),
              Option(company).filter(_.nonEmpty)
            )
          )
        )
    case SignupError(message)   => (setErrorStatus(message), Cmd.None)
    case SignupSuccess(message) => (setSuccessStatus(message), Cmd.None)
    case _                      => (this, Cmd.None)
  }

  override def renderFormContent(): List[Html[App.Msg]] = List(
    renderInput("Email", "email", "text", true, UpdateEmail(_)),
    renderInput("Password", "password", "password", true, UpdatePassword(_)),
    renderInput("Confirm password", "cPassword", "password", true, UpdateConfirmPassword(_)),
    renderInput("FirstName", "firstname", "text", false, UpdateFirstName(_)),
    renderInput("LastName", "lastname", "text", false, UpdateLastName(_)),
    renderInput("Company", "company", "text", false, UpdateCompany(_)),
    button(`type` := "button", onClick(AttempSignUp))("Sign Up")
  )

  def setErrorStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.ERROR)))

  def setSuccessStatus(message: String) =
    this.copy(status = Some(Page.Status(message, Page.StatusKind.SUCCESS)))
}

object SignupPage {
  trait Msg                                                 extends App.Msg
  case class UpdateEmail(email: String)                     extends Msg
  case class UpdatePassword(password: String)               extends Msg
  case class UpdateConfirmPassword(confirmPassword: String) extends Msg
  case class UpdateFirstName(firstName: String)             extends Msg
  case class UpdateLastName(lastName: String)               extends Msg
  case class UpdateCompany(company: String)                 extends Msg

  case object AttempSignUp extends Msg
  case object NoOp         extends Msg

  case class SignupError(message: String)   extends Msg
  case class SignupSuccess(message: String) extends Msg

  object Endpoints {
    val signup = new Endpoint[Msg] {
      val location = Constants.endpoints.signup
      val method   = Method.Post
      val onResponse: Response => Msg = response =>
        response.status match {
          case Status(201, _) =>
            SignupSuccess("Success ! Log in now.")
          case Status(s, _) if s >= 400 & s < 500 =>
            val json   = response.body
            val parsed = parse(json).flatMap(_.hcursor.get[String]("error"))
            parsed match {
              case Left(e)  => SignupError(s"Error: ${e.getMessage}")
              case Right(e) => SignupError(e)
            }
        }
      val onError: HttpError => Msg =
        e => SignupError(e.toString)
    }
  }

  object Commands {
    def signup(newUserInfo: NewUserInfo): Cmd[IO, Msg] = {
      Endpoints.signup.call(newUserInfo)
    }
  }
}
