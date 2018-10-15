package com.bunyod.twofactorauth

import java.security.SecureRandom
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.collection.mutable.StringBuilder


/**
  * Scala implementation for the Time-based One-Time Password (TOTP) two factor authentication algorithm. To get this to
  * work you:
  *
  * <ol>
  * <li>Use generateBase32Secret() to generate a secret key for a user.</li>
  * <li>Store the secret key in the database associated with the user account.</li>
  * <li>Display the QR image URL returned by qrImageUrl(...) to the user.</li>
  * <li>User uses the image to load the secret key into his authenticator application.</li>
  * </ol>
  *
  * <p>
  * Whenever the user logs in:
  * </p>
  *
  * <ol>
  * <li>The user enters the number from the authenticator application into the login form.</li>
  * <li>Read the secret associated with the user account from the database.</li>
  * <li>The server compares the user input with the output from generateCurrentNumber(...).</li>
  * <li>If they are equal then the user is allowed to log in.</li>
  * </ol>
  *
  * <p>
  * Java implementation by graywatson: https://github.com/j256/two-factor-auth
  * </p>
  * * <p>
  * Scala implementation by Bunyod: https://github.com/Bunyod/TwoFactorAuth
  * </p>
  *
  * <p>
  * For more details about this magic algorithm, see: http://en.wikipedia.org/wiki/Time-based_One-time_Password_Algorithm
  * </p>
  *
  * @author Bunyod
  */
class TimeBasedOneTimePasswordUtil {
  /** default time-step which is part of the spec, 30 seconds is default */
  val DefaultTimeStepSeconds = 30
  /** set to the number of digits to control 0 prefix, set to 0 for no prefix */
  private val NumDigitsOutput = 6
  private val blockOfZeros = List.fill[Char](NumDigitsOutput)('0').mkString


  /**
    * Generate and return a 16-character secret key in base32 format (A-Z2-7) using SecureRandom. Could be used
    * to generate the QR image to be shared with the user. Other lengths should use generateBase32Secret(int).
    */
  def generateBase32Secret: String = generateBase32Secret(16)

  /**
    * Similar to generateBase32Secret() but specifies a character length.
    */
  def generateBase32Secret(length: Int): String = {
    def doEval(len: Int, rnd: => Int, res: String): String = if (len < length) {
      val randNum = rnd
      if (randNum < 26) {
        doEval(len + 1, random(), res + ('A'.toInt + randNum).toChar)
      } else {
        doEval(len + 1, random(), res + ('2'.toInt + (randNum - 26)).toChar)
      }
    } else {
      res
    }
    doEval(0, random(), "")
  }

  private def random(): Int = {
    new SecureRandom().nextInt(32)
  }
  /**
    * Validate a given secret-number using the secret base-32 string. This allows you to set a window in milliseconds to
    * account for people being close to the end of the time-step. For example, if windowMillis is 10000 then this method
    * will check the authNumber against the generated number from 10 seconds before now through 10 seconds after now.
    *
    * <p>
    * WARNING: This requires a system clock that is in sync with the world.
    * </p>
    *
    * @param base32Secret
    * Secret string encoded using base-32 that was used to generate the QR code or shared with the user.
    * @param authNumber
    * Time based number provided by the user from their authenticator application.
    * @param windowMillis
    * Number of milliseconds that they are allowed to be off and still match. This checks before and after
    * the current time to account for clock variance. Set to 0 for no window.
    * @return True if the authNumber matched the calculated number within the specified window.
    */
  def validateCurrentNumber(base32Secret: String, authNumber: Int, windowMillis: Int): Boolean =
    validateCurrentNumber(base32Secret, authNumber, windowMillis, System.currentTimeMillis, DefaultTimeStepSeconds)

  /**
    * Similar to validateCurrentNumber(String, int, int) except exposes other parameters. Mostly for testing.
    *
    * @param base32Secret
    * Secret string encoded using base-32 that was used to generate the QR code or shared with the user.
    * @param authNumber
    * Time based number provided by the user from their authenticator application.
    * @param windowMillis
    * Number of milliseconds that they are allowed to be off and still match. This checks before and after
    * the current time to account for clock variance. Set to 0 for no window.
    * @param timeMillis
    * Time in milliseconds.
    * @param timeStepSeconds
    * Time step in seconds. The default value is 30 seconds here. See { @link #DEFAULT_TIME_STEP_SECONDS}.
    * @return True if the authNumber matched the calculated number within the specified window.
    */
  def validateCurrentNumber(base32Secret: String, authNumber: Int, windowMillis: Int, timeMillis: Long, timeStepSeconds: Int): Boolean = {
    var from = timeMillis
    var to = timeMillis
    if (windowMillis > 0) {
      from -= windowMillis
      to += windowMillis
    }
    val timeStepMillis = timeStepSeconds * 1000
    var millis = from
    while ( {
      millis <= to
    }) {
      val compare = generateNumber(base32Secret, millis, timeStepSeconds)
      if (compare == authNumber) return true

      millis += timeStepMillis
    }
    false
  }

  /**
    * Return the current number to be checked. This can be compared against user input.
    *
    * <p>
    * WARNING: This requires a system clock that is in sync with the world.
    * </p>
    *
    * @param base32Secret
    * Secret string encoded using base-32 that was used to generate the QR code or shared with the user.
    * @return A number as a string with possible leading zeros which should match the user's authenticator application
    *         output.
    */
  def generateCurrentNumberString(base32Secret: String): String =
    generateNumberString(base32Secret, System.currentTimeMillis, DefaultTimeStepSeconds)

  /**
    * Similar to #generateCurrentNumberString(String) except exposes other parameters. Mostly for testing.
    *
    * @param base32Secret
    * Secret string encoded using base-32 that was used to generate the QR code or shared with the user.
    * @param timeMillis
    * Time in milliseconds.
    * @param timeStepSeconds
    * Time step in seconds. The default value is 30 seconds here. See DefaultTimeStepSeconds.
    * @return A number as a string with possible leading zeros which should match the user's authenticator application
    *         output.
    */
  def generateNumberString(base32Secret: String, timeMillis: Long, timeStepSeconds: Int): String = {
    val number = generateNumber(base32Secret, timeMillis, timeStepSeconds)
    zeroPrepend(number, NumDigitsOutput)
  }

  /**
    * Similar to generateCurrentNumberString(String) but this returns a long instead of a string.
    *
    * @return A number which should match the user's authenticator application output.
    */
  def generateCurrentNumber(base32Secret: String): Long =
    generateNumber(base32Secret, System.currentTimeMillis, DefaultTimeStepSeconds)

  /**
    * Similar to generateNumberString(String, long, int) but this returns a long instead of a string.
    *
    * @return A number which should match the user's authenticator application output.
    */
  def generateNumber(base32Secret: String, timeMillis: Long, timeStepSeconds: Int): Long = {
    val key = Base32.encode(base32Secret)
    val value = timeMillis / 1000 / timeStepSeconds

    def fillData(value: Long, counter: Int, data: Array[Byte]): Array[Byte] = {
      if (counter > 0) {
        fillData( value >> 8, counter - 1, (value & 0xFF).toByte +: data)
      } else {
        data
      }
    }
    val data = fillData(value, 7, new Array[Byte](8))

    // encrypt the data with the key and return the SHA1 of it in hex
    val signKey = new SecretKeySpec(key, "HmacSHA1")
    // if this is expensive, could put in a thread-local
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(signKey)


    val hash = mac.doFinal(data)
    // take the 4 least significant bits from the encrypted string as an offset
    val offset = hash(hash.length - 1) & 0xF


    def trncHash(counter: Int, truncatedHash: Long): Long = {
      if (counter < offset + 4) {
        trncHash(counter + 1, (truncatedHash << 8) | (hash(counter) & 0xFF))
      } else {
        truncatedHash
      }
    }

    // cut off the top bit
    val truncatedHash = trncHash(offset, 0) & 0x7FFFFFFF
    // the token is then the last 6 digits in the number
    truncatedHash % 1000000
  }

  /**
    * Return the QR image url thanks to Google. This can be shown to the user and scanned by the authenticator program
    * as an easy way to enter the secret.
    *
    * @param keyId
    * Name of the key that you want to show up in the users authentication application. Should already be
    * URL encoded.
    * @param secret
    * Secret string that will be used when generating the current number.
    */
  def qrImageUrl(keyId: String, secret: String): String = {
    val sb = new StringBuilder(128)
    sb.append("https://chart.googleapis.com/chart?chs=200x200&cht=qr&chl=200x200&chld=M|0&cht=qr&chl=")
    addOtpAuthPart(keyId, secret, sb)
    sb.toString
  }

  /**
    * Return the otp-auth part of the QR image which is suitable to be injected into other QR generators (e.g. JS
    * generator).
    *
    * @param keyId
    * Name of the key that you want to show up in the users authentication application. Should already be
    * URL encoded.
    * @param secret
    * Secret string that will be used when generating the current number.
    */
  def generateOtpAuthUrl(keyId: String, secret: String): String = {
    val sb = new StringBuilder(64)
    addOtpAuthPart(keyId, secret, sb)
    sb.toString
  }

  private def addOtpAuthPart(keyId: String, secret: String, sb: StringBuilder): Unit = {
    sb.append("otpauth://totp/").append(keyId).append("%3Fsecret%3D").append(secret)
  }

  /**
    * Return the string prepended with 0s. Tested as 10x faster than String.format("%06d", ...); Exposed for testing.
    */
  def zeroPrepend(num: Long, digits: Int): String = {
    val numStr = num.toString
    if (numStr.length >= digits) numStr
    else {
      val sb = new StringBuilder(digits)
      val zeroCount = digits - numStr.length
      sb.append(blockOfZeros, 0, zeroCount)
      sb.append(numStr)
      sb.toString
    }
  }

  /**
    * Decode base-32 method. I didn't want to add a dependency to Apache Codec just for this decode method. Exposed for
    * testing.
    */

}
