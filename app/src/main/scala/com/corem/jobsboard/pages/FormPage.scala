package com.corem.jobsboard.pages

import tyrian.*
import tyrian.Html.*
import tyrian.http.*

import com.corem.jobsboard.*
import cats.effect.IO
import com.corem.jobsboard.App.Msg
import com.corem.jobsboard.core.Router

abstract class FormPage(title: String, status: Option[Page.Status]) extends Page {

  protected def renderFormContent(): List[Html[App.Msg]]

  override def initCmd: Cmd[IO, Msg] =
    Cmd.None

  override def view(): Html[Msg] =
    renderForm()

  protected def renderForm(): Html[App.Msg] =
    div(`class` := "form-section")(
      div(`class` := "top-section")(
        h1(title)
      ),
      form(
        name    := "signin",
        `class` := "form",
        onEvent(
          "submit",
          e => {
            e.preventDefault()
            App.NoOp
          }
        )
      )(
        renderFormContent()
      ),
      status.map(s => div(s.message)).getOrElse(div())
    )

  protected def renderInput(
      name: String,
      uid: String,
      kind: String,
      isRequired: Boolean,
      onChange: String => Msg
  ) =
    div(`class` := "form-input")(
      label(`for` := name, `class` := "form-label")(
        if (isRequired) span("*") else span(),
        text(name)
      ),
      input(`type` := kind, `class` := "form-control", id := uid, onInput(onChange))
    )

  protected def renderAuxLink(location: String, text: String): Html[App.Msg] =
    a(
      href    := location,
      `class` := "aux-link",
      onEvent(
        "click",
        e => {
          e.preventDefault()
          Router.ChangeLocation(location)
        }
      )
    )(text)
}
