package com.corem.jobsboard.pages

import tyrian.*
import cats.effect.*

import com.corem.jobsboard.*
import com.corem.jobsboard.components.*

object Page {
  trait Msg

  enum StatusKind {
    case SUCCESS, ERROR, LOADING
  }

  case class Status(message: String, kind: StatusKind)
  object Status {
    val LOADING = Status("Loading", StatusKind.LOADING)
  }

  object Urls {
    val EMPTY           = ""
    val HOME            = "/"
    val LOGIN           = "/login"
    val SIGNUP          = "/signup"
    val FORGOT_PASSWORD = "/forgotpassword"
    val RESET_PASSWORD  = "/resetpassword"
    val PROFILE         = "/profile"
    val POST_JOB        = "/postjob"
    val JOBS            = "/jobs"
    val HASH            = "#"
    def JOB(id: String) = s"/jobs/$id"
  }

  import Urls.*
  def get(location: String) = location match {
    case `LOGIN`                   => LoginPage()
    case `SIGNUP`                  => SignupPage()
    case `FORGOT_PASSWORD`         => ForgotPasswordPage()
    case `RESET_PASSWORD`          => ResetPasswordPage()
    case `PROFILE`                 => ProfilePage()
    case `POST_JOB`                => PostJobPage()
    case `EMPTY` | `HOME` | `JOBS` => JobsListPage()
    case s"/jobs/$id"              => JobPage(id)
    case _                         => NotFoundPage()
  }
}

abstract class Page extends Component[App.Msg,Page]
