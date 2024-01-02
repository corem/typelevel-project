package com.corem.jobsboard.pages

import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*

import cats.effect.IO

import com.corem.jobsboard.*

class JobsListPage extends Page {
  override def initCmd: Cmd[IO, App.Msg] =
    Cmd.None
  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) =
    (this, Cmd.None)
  override def view(): Html[App.Msg] =
    div("Jobs List Page - TODO")
}
