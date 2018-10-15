import com.bunyod.twofactorauth.TimeBasedOneTimePasswordUtil

object Boot extends App {

  println("Hello")

  val tmotp = new TimeBasedOneTimePasswordUtil()
  val seckey = tmotp.generateBase32Secret
  println(seckey)

  val r = tmotp.qrImageUrl("test", seckey)
  println(r)

}
