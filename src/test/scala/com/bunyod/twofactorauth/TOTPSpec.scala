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
        //    testStringAndNumber(secret, 348402049542546145L, 9637, "009637")
        //    testStringAndNumber(secret, 2049455124374752571L, 743, "000743")
        //    testStringAndNumber(secret, 1359002349304873750L, 92, "000092")
        //    testStringAndNumber(secret, 6344447817348357059L, 7, "000007")
        //    testStringAndNumber(secret, 2125701285964551130L, 0, "000000")
      }
    }
  }
  private def testStringAndNumber(secret: String, timeMillis: Long, expectedNumber: Long, expectedString: String): Unit = {
    assert(expectedString == TOTP.generateNumberString(secret, timeMillis, TOTP.DefaultTimeStepSeconds))
    assert(expectedNumber == TOTP.generateNumber(secret, timeMillis, TOTP.DefaultTimeStepSeconds))
  }
}
