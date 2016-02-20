package me.krobinson.mealplan.generators

import java.net.URL

import me.krobinson.mealplan.model.Pin
import org.scalacheck.Gen, Gen._

object `package` {

  def urlGen: Gen[URL] =
    for {
      protocol <- oneOf("http", "https")
      host     <- alphaStr
      tld      <- oneOf("io", "com", "net")
    } yield new URL(s"$protocol://www.$host.$tld")

  def genPin: Gen[Pin] =
    for {
      note <- alphaStr
      link <- urlGen
      id   <- posNum[Int].map(_.toString)
    } yield Pin(note, link, id)

  def genPinList(size: Int) = Gen.listOfN(size, genPin)
}