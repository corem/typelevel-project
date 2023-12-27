package com.corem.jobsboard.config

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

final case class TokenConfig(tokenDuration: Long) derives ConfigReader
