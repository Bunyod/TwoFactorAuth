package com.bunyod.twofactorauth

import org.scalatest._

class TOTPSpec extends AsyncWordSpec with Matchers {

  "The TOTP Util" when {
    "called with pre defined secret key" should {
      "return true" in {

        val secret = "NY4A5CPJZ46LXZCP"
        assert(TOTP.generateNumberString(secret, 1000L, TOTP.DefaultTimeStepSeconds) == "748810")
        assert(TOTP.generateNumber(secret, 1000L, TOTP.DefaultTimeStepSeconds) == 748810)
        assert(TOTP.generateNumberString(secret, 7451000L, TOTP.DefaultTimeStepSeconds) == "325893")
        assert(TOTP.generateNumber(secret, 7451000L, TOTP.DefaultTimeStepSeconds) == 325893)
        assert(TOTP.generateNumberString(secret, 15451000L, TOTP.DefaultTimeStepSeconds) == "064088")
        assert(TOTP.generateNumber(secret, 15451000L, TOTP.DefaultTimeStepSeconds) == 64088)
        assert(TOTP.generateNumberString(secret, 348402049542546145L,  TOTP.DefaultTimeStepSeconds) == "009637")
        assert(TOTP.generateNumber(secret, 348402049542546145L,  TOTP.DefaultTimeStepSeconds) == 9637)
        assert(TOTP.generateNumberString(secret, 2049455124374752571L, TOTP.DefaultTimeStepSeconds) == "000743")
        assert(TOTP.generateNumber(secret, 2049455124374752571L, TOTP.DefaultTimeStepSeconds) == 743)
        assert(TOTP.generateNumberString(secret, 1359002349304873750L, TOTP.DefaultTimeStepSeconds) == "000092")
        assert(TOTP.generateNumber(secret, 1359002349304873750L, TOTP.DefaultTimeStepSeconds) ==  92)
        assert(TOTP.generateNumberString(secret, 6344447817348357059L, TOTP.DefaultTimeStepSeconds) == "000007")
        assert(TOTP.generateNumber(secret, 6344447817348357059L, TOTP.DefaultTimeStepSeconds) ==  7)
        assert(TOTP.generateNumberString(secret, 2125701285964551130L, TOTP.DefaultTimeStepSeconds) == "000000")
        assert(TOTP.generateNumber(secret, 2125701285964551130L, TOTP.DefaultTimeStepSeconds) == 0)
      }
    }
  }

//  private def testStringAndNumber(secret: String, timeMillis: Long, expectedNumber: Long, expectedString: String): Unit = {
//    assert(expectedString == TOTP.generateNumberString(secret, timeMillis, TOTP.DefaultTimeStepSeconds))
//    assert(expectedNumber == TOTP.generateNumber(secret, timeMillis, TOTP.DefaultTimeStepSeconds))
//  }
}
