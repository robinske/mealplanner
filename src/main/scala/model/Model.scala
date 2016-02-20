package me.krobinson.mealplan.model

import java.net.URL

import scalaz.\/

object `package` {
  type Result[A] = \/[String, A]
}

case class ApiResponse[A](data: A, nextPage: Option[String])

case class Counts(pins: Int = 0)

case class Board(name: String, description: String, id: String, url: URL, counts: Counts)

case class Pin(note: String, link: URL, id: String)