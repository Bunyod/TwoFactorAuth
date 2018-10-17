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

}