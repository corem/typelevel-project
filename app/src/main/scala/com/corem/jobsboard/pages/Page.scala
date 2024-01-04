package com.corem.jobsboard.pages

import tyrian.*
import cats.effect.*

import com.corem.jobsboard.*

object Page {
  trait Msg

  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }

  case class Status(message: String, kind: StatusKind)

  object Urls {
    val EMPTY           = ""
    val HOME            = "/"
    val LOGIN           = "/login"
    val SIGNUP          = "/signup"
    val FORGOT_PASSWORD = "/forgotpassword"
    val RESET_PASSWORD  = "/resetpassword"
    val PROFILE         = "/profile"
    val JOBS            = "/jobs"
    val HASH            = "#"
  }

  import Urls.*
  def get(location: String) = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignupPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RESET_PASSWORD`          => ResetPasswordPage()
    case `PROFILE`                 => ProfilePage()
    case `EMPTY` | `HOME` | `JOBS` => JobsListPage()
    case s"/jobs/$id"              => JobPage(id)
    case _                         => NotFoundPage()
  }
}

abstract class Page {
  import App.Msg

  def initCmd: Cmd[IO, Msg]

  def update(msg: Msg): (Page, Cmd[IO, Msg])

  def view(): Html[Msg]
}
