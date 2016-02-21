package me.krobinson.mealplan.model

import java.net.URL

import argonaut.{DecodeResult, HCursor, Json}

import scalaz.{\/-, \/}

object `package` {
  type Result[A] = \/[String, A]

  type Days = (Int, Int, Int, Int, Int, Int, Int)
}

case class ApiResponse[A](data: A, nextPage: Option[String])

case class Counts(pins: Int = 0)

case class Board(
  name: String,
  description: String,
  id: String,
  url: URL,
  counts: Counts
)

sealed trait Media { def `type`: String }
case class Image(url: URL, width: Int, height: Int) extends Media { def `type`: String = "image" }
case object Video extends Media { def `type`: String = "video" }

object Media {
  import me.krobinson.mealplan.model.json._

  def apply(hc: HCursor): DecodeResult[Media] =
    (hc --\ "media" --\ "type").as[String] match {
      case DecodeResult(\/-("image")) => hc.jdecode[Image]
      case DecodeResult(\/-("video")) => DecodeResult.ok(Video)
      case _ => DecodeResult.fail("Unexpected media type", hc.history)
    }
}

case class Recipe(
  note: String,
  link: URL,
  id: String,
  media: Media,
  metadata: Json
)