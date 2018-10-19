package com.bunyod.twofactorauth

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
    * Decode base-32 method.
    */
  def decodeBase32(str: String): List[Byte] = {

    case class Dec(which: Int, working: Int, result: List[Byte])

    def go(counter: Int, dec: Dec): Dec = {
      if (counter < str.length) {
        val ch: Char = str.charAt(counter)
        val value: Int = if (ch >= 'a' && ch <= 'z') {
          ch - 'a'
        } else if (ch >= 'A' && ch <= 'Z') {
          ch - 'A'
        } else if (ch >= '2' && ch <= '7') {
          26 + (ch - '2')
        } else {
          throw new IllegalArgumentException("Invalid base-32 character: " + ch)
        }

        if (ch == '=') {
          dec
        } else {
          dec.which match {
            case 0 =>
              go(counter + 1, dec.copy(which = 1, working = (value & 0x1F) << 3))
            case 1 =>
              val wk = dec.working | (value & 0x1C) >> 2
              go(counter + 1, dec.copy(2, (value & 0x03) << 6, dec.result :+ wk.toByte))
            case 2 =>
              val wk = dec.working | (value & 0x1F) << 1
              go(counter + 1, dec.copy(3, working = wk))
            case 3 =>
              val wk = dec.working | (value & 0x10) >> 4
              go(counter + 1, dec.copy(4, (value & 0x0F) << 4, dec.result :+ wk.toByte))
            case 4 =>
              val wk = dec.working | (value & 0x1E) >> 1
              go(counter + 1, dec.copy(5, (value & 0x01) << 7, dec.result :+ wk.toByte))
            case 5 =>
              val wk = dec.working | (value & 0x1F) << 2
              go(counter + 1, dec.copy(6, wk))
            case 6 =>
              val wk = dec.working | (value & 0x18) >> 3
              go(counter + 1, dec.copy(7, (value & 0x07) << 5, dec.result :+ wk.toByte))
            case 7 =>
              val wk = dec.working | (value & 0x1F)
              go(counter + 1, dec.copy(0, wk, dec.result :+ wk.toByte))
          }
        }

      } else {
        dec
      }
    }

    val dec = go(0, Dec(0, 0, List.empty[Byte]))
    val (which, working, result) = Dec.unapply(dec).getOrElse(0, 0, Nil)

    val numBytes: Int = ((str.length * 5) + 7) / 8

    if (which != 0) {
      result :+ working.toByte
    } else if (result.size != numBytes) {
      result ++ List.fill(numBytes - result.size)("".toByte)
    } else {
      result
    }
  }
}