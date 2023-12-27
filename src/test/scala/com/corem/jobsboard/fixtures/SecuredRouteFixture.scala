package com.corem.jobsboard.fixtures

import cats.data.*
import cats.effect.*
import tsec.authentication.JWTAuthenticator
import tsec.authentication.IdentityStore
import tsec.mac.jca.HMACSHA256
import scala.concurrent.duration.*
import org.http4s.*
import org.http4s.headers.*
import tsec.jws.mac.JWTMac
import org.http4s.Credentials

import com.corem.jobsboard.domain.user.*
import com.corem.jobsboard.domain.security.*
import tsec.authentication.SecuredRequestHandler

trait SecuredRouteFixture extends UserFixture {
  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == remiEmail) OptionT.pure(Remi)
      else if (email == gastonEmail) OptionT.pure(Gaston)
      else OptionT.none[IO, User]
    JWTAuthenticator.unbacked.inBearerToken(
      1.day,   // Token expiration
      None,    // Max idle time (optional)
      idStore, // Id Store
      key      // Hash key
    )
  }

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] = {
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)
        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }
    }
  
  given securedHandler: SecuredHandler[IO] = SecuredRequestHandler(mockedAuthenticator)
}
