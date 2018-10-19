import com.bunyod.twofactorauth.TOTP

object Boot extends App {

  println("Hello")

  val input = io.StdIn.readLong()
//  val seckey = TOTP.generateBase32Secret
  val seckey = "HDCEFFQGDEWETKGK"
//  println(TOTP.generateBase32Secret)
//  println(seckey)

//  val r = TOTP.qrImageUrl("test", seckey)
//  println(r)
  println(TOTP.generateCurrentNumber(seckey))
  println(TOTP.generateCurrentNumberString(seckey))

}
