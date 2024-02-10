package com.corem.jobsboard.components

import tyrian.*
import tyrian.Html.*

import com.corem.jobsboard.*

object Footer {
  def view(): Html[App.Msg] =
    div(`class` := "footer")(
      p(
        text("Written in "),
        a(href := "https://scala-lang.org", target := "blank")("Scala"),
        text(" with <3")
      )
    )
}
