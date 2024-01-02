package com.corem.jobsboard.playground

import cats.effect.IOApp
import cats.effect.IO
import tsec.passwordhashers.jca.BCrypt
import tsec.passwordhashers.PasswordHash

object PasswordHashingPlayground extends IOApp.Simple {

  override def run: IO[Unit] =
    BCrypt.hashpw[IO]("remimypassword").flatMap(IO.println) *>
      BCrypt.hashpw[IO]("gastonmypassword").flatMap(IO.println) *>
      BCrypt.hashpw[IO]("newusermypassword").flatMap(IO.println) *>
      BCrypt.hashpw[IO]("gastonmynewpassword").flatMap(IO.println) *> 
      BCrypt.hashpw[IO]("remimynewpassword").flatMap(IO.println)
      /*     BCrypt
      .checkpwBool[IO](
        "test",
        PasswordHash[BCrypt]("$2a$10$0EfDzFBSofVSlRwTx033r.RXQ3XEG3EP/AFNwF8m6JXKuKybHq7kC")
      )
      .flatMap(IO.println) */
}