package me.krobinson.mealplan.model

import java.net.URL

sealed trait Result[+A]
case class Data[A](data: A) extends Result[A]
case class Fail(message: String) extends Result[Nothing]


case class Counts(pins: Int = 0)

case class Board(name: String, description: String, id: String, url: URL, counts: Counts)
case class Pin(note: String, link: URL, id: String)