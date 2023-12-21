package com.corem.jobsboard.fixtures

import cats.effect.IO

import com.corem.jobsboard.core.Users
import com.corem.jobsboard.domain.user.*

trait UserFixture {

  val mockedUsers: Users[IO] = new Users[IO] {
    override def find(email: String): IO[Option[User]] =
      if (email == remiEmail) IO.pure(Some(Remi))
      else IO.pure(None)
    override def create(user: User): IO[String]       = IO.pure(user.email)
    override def update(user: User): IO[Option[User]] = IO.pure(Some(user))
    override def delete(email: String): IO[Boolean]   = IO.pure(true)
  }

  val Remi = User(
    "cornet.remi@corem.corp",
    "$2a$10$jY60jL/9Lv6./UHhhj2ZvOSm8PQIiTueC4gmsegrD5K.Yi6/mGY.m",
    Some("Remi"),
    Some("Cornet"),
    Some("Corem Corp"),
    Role.ADMIN
  )
  val remiEmail    = Remi.email
  val remiPassword = "hashedpassword"

  val Gaston = User(
    "gaston.elgato@corem.corp",
    "$2a$10$jDPXCNCHkbZLzmiTRuw9A.gBHRDQ1iKnYONCBuskyOln8Aa8eucFa",
    Some("Gaston"),
    Some("El Gato"),
    Some("Corem Corp"),
    Role.RECRUITER
  )
  val gastonEmail    = Gaston.email
  val gastonPassword = "hashedpassword"

  val NewUser = User(
    "newuser@gmail.com",
    "$2a$10$6LQt4xy4LzqQihZiRZGG0eeeDwDCvyvthICXzPKQDQA3C47LtrQFy",
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val UpdatedGaston = User(
    "gaston.elgato@corem.corp",
    "$2a$10$PUD6CznGVHntJFsOOeV4NezBgBUs6irV3sC9fa6ufc0xp9VLYyHZ.",
    Some("GASTON"),
    Some("EL GATO"),
    Some("Adobe"),
    Role.RECRUITER
  )
}