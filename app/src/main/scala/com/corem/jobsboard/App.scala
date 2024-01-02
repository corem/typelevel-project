package com.corem.jobsboard

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.concurrent.duration.*
import org.scalajs.dom.{window, console}
import scala.scalajs.js.annotation.JSExport
import cats.effect.IO

import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger

import com.corem.jobsboard.core.*
import com.corem.jobsboard.components.*
import com.corem.jobsboard.pages.Page

object App {
  trait Msg
  case class Model(router: Router, session: Session, page: Page)
}

@JSExportTopLevel("CoremApp")
class App extends TyrianApp[App.Msg, App.Model] {
  import App.*
  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    val location            = window.location.pathname
    val page                = Page.get(location)
    val pageCmd             = page.initCmd
    val (router, routerCmd) = Router.startAt(location)
    val session             = Session()
    val sessionCmd          = session.initCmd
    (Model(router, session, page), routerCmd |+| sessionCmd |+| pageCmd)
  }

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.make(
      "urlChange",
      model.router.history.state.discrete
        .map(_.get)
        .map(newLocation => Router.ChangeLocation(newLocation, true))
    )

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case msg: Router.Msg =>
      val (newRouter, routerCmd) = model.router.update(msg)
      if (model.router == newRouter) (model, Cmd.None)
      else {
        val newPage    = Page.get(newRouter.location)
        val newPageCmd = newPage.initCmd
        (model.copy(router = newRouter, page = newPage), routerCmd |+| newPageCmd)
      }
    case msg: Session.Msg =>
      val (newSession, cmd) = model.session.update(msg)
      (model.copy(session = newSession), cmd)
    case msg: App.Msg =>
      val (newPage, cmd) = model.page.update(msg)
      (model.copy(page = newPage), cmd)
  }

  override def view(model: Model): Html[Msg] =
    div(
      Header.view(),
      model.page.view(),
      div(model.session.email.getOrElse("Unauthenticated"))
    )
}
