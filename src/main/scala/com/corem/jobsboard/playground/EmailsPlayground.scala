package com.corem.jobsboard.playground

import java.util.Properties
import javax.mail.PasswordAuthentication
import javax.mail.Authenticator
import javax.mail.Session
import javax.mail.internet.MimeMessage
import javax.mail.Message
import javax.mail.Transport
import cats.effect.IOApp
import cats.effect.IO

import com.corem.jobsboard.config.*
import com.corem.jobsboard.core.*

object EmailsPlayground {
  def main(args: Array[String]): Unit = {
    val host        = "smtp.ethereal.email"
    val port        = 587
    val user        = "jordyn.jast@ethereal.email"
    val password    = "FP2hNPdSSY3r9t4KAy"
    val token       = "ABCD1234"
    val frontendUrl = "http://google.com"

    val prop = new Properties
    prop.put("mail.smtp.auth", true)
    prop.put("mail.smtp.starttls.enable", true)
    prop.put("mail.smtp.host", host)
    prop.put("mail.smtp.port", port)
    prop.put("mail.smtp.ssl.trust", host)

    val auth = new Authenticator {
      override protected def getPasswordAuthentication(): PasswordAuthentication =
        new PasswordAuthentication(user, password)
    }

    val session = Session.getInstance(prop, auth)

    val subject = "Email from Corem Corp"
    val content = s"""
    <div style="
        border: 1px solid black;
        padding: 20px;
        font-family: sans-serif;
        line-height: 2;
        font-size: 20px;
    ">
    <h1>Email from Corem Corp</h1>
    <p>Your password recovery token is: $token</p>
    <p>
        Click <a href="$frontendUrl/login">here</a> to get back to the application
    </p>
    </div>
    """

    val message = new MimeMessage(session)
    message.setFrom("corem@corem.corp")
    message.setRecipients(Message.RecipientType.TO, "the.user@gmail.com")
    message.setSubject(subject)
    message.setContent(content, "text/html; charset=utf-8")

    Transport.send(message)
  }
}

object EmailsEffectPlayground extends IOApp.Simple {
  override def run: IO[Unit] = for {
    emails <- LiveEmails[IO](
      EmailServiceConfig(
        host = "smtp.ethereal.email",
        port = 587,
        user = "jordyn.jast@ethereal.email",
        pass = "FP2hNPdSSY3r9t4KAy",
        frontendUrl = "http://google.com"
      )
    )
    _ <- emails.sendPasswordRecoveryEmail("someone@corem.corp", "Token123")
  } yield ()
}
