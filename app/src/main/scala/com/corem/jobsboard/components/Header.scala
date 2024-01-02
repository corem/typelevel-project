package com.corem.jobsboard.components

import tyrian.*
import tyrian.Html.*
import scala.scalajs.js
import scala.scalajs.js.annotation.*

import com.corem.jobsboard.*
import com.corem.jobsboard.core.*
import com.corem.jobsboard.pages.*

object Header {

  def view() = {
    div(`class` := "header-container")(
      renderLogo(),
      div(`class` := "header-nav")(
        ul(`class` := "header-links")(
          renderNavLinks()
        )
      )
    )
  }

  @js.native
  @JSImport("/static/img/logocorgi.jpg", JSImport.Default)
  private val logoImage: String = js.native

  private def renderLogo() = {
    a(
      href := Page.Urls.HOME,
      onEvent(
        "click",
        e => {
          e.preventDefault()
          Router.ChangeLocation("/")
        }
      )
    )(
      img(
        `class` := "home-logo",
        src     := logoImage,
        alt     := "Corem Logo"
      )
    )
  }

  private def renderNavLinks() = {
    val constantLinks: List[Html[App.Msg]] = List(
      renderSimpleNavLink("Jobs", Page.Urls.JOBS)
    )

    val unauthedLinks = List(
      renderSimpleNavLink("Login", Page.Urls.LOGIN),
      renderSimpleNavLink("Sign Up", Page.Urls.SIGNUP)
    )

    val authedLinks = List(
      renderNavLink("Logout", Page.Urls.HASH)(_ => Session.Logout)
    )

    constantLinks ++ (
      if (Session.isActive) authedLinks
      else unauthedLinks
    )
  }

  private def renderSimpleNavLink(text: String, location: String) =
    renderNavLink(text, location)(Router.ChangeLocation(_))

  private def renderNavLink(text: String, location: String)(locationToMsg: String => App.Msg) =
    li(`class` := "nav-item")(
      a(
        href    := location,
        `class` := "nav-link",
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
