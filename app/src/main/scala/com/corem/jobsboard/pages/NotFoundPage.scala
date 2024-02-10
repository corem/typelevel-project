package com.corem.jobsboard.pages

import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*

import cats.effect.IO

import com.corem.jobsboard.*
import com.corem.jobsboard.common.Constants

class NotFoundPage extends Page {
  override def initCmd: Cmd[IO, App.Msg] =
    Cmd.None
  override def update(msg: App.Msg): (Page, Cmd[IO, App.Msg]) =
    (this, Cmd.None)
  override def view(): Html[App.Msg] =
    div(`class` := "row")(
      div(`class` := "col-md-5 p-0")(
        div(`class` := "logo")(
          img(src   := Constants.logoImage)
        )
      ),
      div(`class` := "col-md-7")(
        div(`class` := "form-section")(
          div(`class` := "top-section")(
            h1(span("Ouch !")),
            div("This page doesn't exist.")
          )
        )
      )
    )
}
