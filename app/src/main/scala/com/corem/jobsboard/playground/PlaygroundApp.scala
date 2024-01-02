package com.corem.jobsboard

import scala.scalajs.js.annotation.JSExportTopLevel
import scala.concurrent.duration.*
import org.scalajs.dom.{document, console}
import scala.scalajs.js.annotation.JSExport
import cats.effect.IO

import tyrian.*
import tyrian.Html.*
import tyrian.cmds.Logger
import com.corem.jobsboard.PlaygroundApp.Model
import com.corem.jobsboard.PlaygroundApp.Msg
import com.corem.jobsboard.PlaygroundApp.Increment

object PlaygroundApp {
  trait Msg
  case class Increment(amount: Int) extends Msg

  case class Model(count: Int)
}

// @JSExportTopLevel("CoremApp")
class PlaygroundApp extends TyrianApp[PlaygroundApp.Msg, PlaygroundApp.Model] {
  override def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (Model(0), Cmd.None)

  override def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
    // Sub.every[IO](1.second).map(_ => Increment(1))

  override def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case Increment(amount) =>
      (model.copy(count = model.count + amount), Logger.consoleLog[IO](s"Changing count by $amount"))
  }

  override def view(model: Model): Html[Msg] =
    div(
      button(onClick(Increment(1)))("Increase !"),
      button(onClick(Increment(-1)))("Decrease !"),
      div(s"Tyrian running: ${model.count}")
    )
}
