package com.bunyod.twofactorauth

import java.util

object Base32 {
  /** Take string, convert to byte array, break into 5 bit groups,
    * and encode each group as a character. (Due to left padding it seems
    * that decoding will only be possible from right to left.)
    */
  def encode(s: String): Array[Byte] = {
    val bytes = s.getBytes
    (List.fill(5 - ((bytes.size*8) % 5))(0) ++ /* left padding with zero bits */ expandBinary(bytes))
      .grouped(5)
      .map(bits => printable(collapseBinary(bits)).toByte)
      .toArray
  }

  /** Expand byte array to list of bits. */
  private def expandBinary(a: Array[Byte]): List[Int] =
    for {
      byt <- a.toList
      bit <- 7 to (0, -1)
    } yield (byt >> bit) & 1

  /** Convert list of bits (not more than 32), most significant first, to an Int.
    */
  private def collapseBinary(bits: List[Int]): Int =
    bits.foldRight((0,1)) { case (b,(acc,value)) => (acc|(b*value),value << 1) } match {
      case (res, _) => res
    }

  /** Convert base-32 digit to a printable character. */
  private def printable(x: Int): Char =
    (x + (if(x < 26) 'a'.toInt else '0'.toInt - 26)).toChar


  /**
    * Decode base-32 method. I didn't want to add a dependency to Apache Codec just for this decode method. Exposed for
    * testing.
    */

  // TODO refactor
  def decodeBase32(str: String): Array[Byte] = {
    val numBytes: Int = ((str.length * 5) + 7) / 8
    var result: Array[Byte] = Array.ofDim[Byte](numBytes)
    var resultIndex: Int = 0
    var which: Int = 0
    var working: Int = 0
    for (i <- // each base-32 character encodes 5 bits
           0 until str.length) {
      val ch: Char = str.charAt(i)
      var value: Int = 0
      if (ch >= 'a' && ch <= 'z') {
        value = ch - 'a'
      } else if (ch >= 'A' && ch <= 'Z') {
        value = ch - 'A'
      } else if (ch >= '2' && ch <= '7') {
        value = 26 + (ch - '2')
      } else if (ch == '=') {
        // special case
        which = 0
        //break
      } else {
        throw new IllegalArgumentException("Invalid base-32 character: " + ch)
      }
      /*
			 * There are probably better ways to do this but this seemed the most straightforward.
			 */

      which match {
        case 0 =>
          // all 5 bits is top 5 bits
          working = (value & 0x1F) << 3
          which = 1
        case 1 =>
          // top 3 bits is lower 3 bits
          working |= (value & 0x1C) >> 2
          result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
          // lower 2 bits is upper 2 bits
          working = (value & 0x03) << 6
          which = 2
        case 2 =>
          // all 5 bits is mid 5 bits
          working |= (value & 0x1F) << 1
          which = 3
        case 3 =>
          // top 1 bit is lowest 1 bit
          working |= (value & 0x10) >> 4
          result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
          // lower 4 bits is top 4 bits
          working = (value & 0x0F) << 4
          which = 4
        case 4 =>
          // top 4 bits is lowest 4 bits
          working |= (value & 0x1E) >> 1
          result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
          // lower 1 bit is top 1 bit
          working = (value & 0x01) << 7
          which = 5
        case 5 =>
          // all 5 bits is mid 5 bits
          working |= (value & 0x1F) << 2
          which = 6
        case 6 =>
          // top 2 bits is lowest 2 bits
          working |= (value & 0x18) >> 3
          result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
          // lower 3 bits of byte 6 is top 3 bits
          working = (value & 0x07) << 5
          which = 7
        case 7 =>
          // all 5 bits is lower 5 bits
          working |= (value & 0x1F)
          result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
          which = 0

      }
    }
    if (which != 0) {
      result({ resultIndex += 1; resultIndex - 1 }) = working.toByte
    }
    if (resultIndex != result.length) {
      result = util.Arrays.copyOf(result, resultIndex)
    }
    result
  }
}