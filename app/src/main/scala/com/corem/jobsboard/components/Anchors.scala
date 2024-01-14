package com.corem.jobsboard.components

import tyrian.*
import tyrian.Html.*

import com.corem.jobsboard.core.*
import com.corem.jobsboard.*

object Anchors {
  def renderSimpleNavLink(text: String, location: String, cssClass: String = "") =
    renderNavLink(text, location, cssClass)(Router.ChangeLocation(_))

  def renderNavLink(text: String, location: String, cssClass: String = "")(locationToMsg: String => App.Msg) =
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := cssClass,
        onEvent(
          "click",
          e => {
            e.preventDefault()
            locationToMsg(location)
          }
        )
      )(text)
    )

}
