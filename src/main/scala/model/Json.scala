package me.krobinson.mealplan.model

import java.net.URL

import argonaut._, Argonaut._, DecodeResult._
import me.krobinson.mealplan.MealPlan

import scala.util.{Success, Try}
import scalaz.{\/, -\/, \/-}
import scalaz.syntax.id._

package object json {

  def handleJson[A, B]
  (decoded: \/[String \/ (String, CursorHistory), A])
  (success: A => B): Result[B] = decoded match {
    case \/-(ar) => success(ar).right
    case -\/(\/-((failure,ch))) =>
      s"""Failure occured when decoding JSON response body: $failure at: $ch""".left
    case -\/(-\/(msg)) => s"Invalid JSON response".left
  }

  def errorJson(message: String): Json =
    ("error" := message) ->: jEmptyObject

  implicit def urlCodec: CodecJson[URL] = CodecJson(
    url => url.toString.asJson,
    hc =>
      hc.as[String] flatMap { urlStr =>
        Try(new URL(urlStr)) match {
          case Success(u) => ok(u)
          case _          => fail(s"Failed to parse link as URL $urlStr", hc.history)
        }
      }
  )

  implicit def boardPinsCodec: CodecJson[ApiResponse[List[Recipe]]] = CodecJson(
    ar =>
      ("data" := ar.data) ->:
      ("page" := ("next" :=? ar.nextPage) ->?: jEmptyObject) ->:
      jEmptyObject,
    hc =>
      for {
        d <- (hc --\ "data").as[List[Recipe]]
        p <- (hc --\ "page" --\ "next").as[Option[String]]
      } yield ApiResponse(d, p)
  )

  implicit def boardMetaCodec: CodecJson[ApiResponse[Board]] = CodecJson(
    ar =>
      ("data" := ar.data) ->:
      ("page" := ("next" :=? ar.nextPage) ->?: jEmptyObject) ->:
      jEmptyObject,
    hc =>
      for {
        d <- (hc --\ "data").as[Board]
        p <- (hc --\ "page" --\ "next").as[Option[String]]
      } yield ApiResponse(d, p)
  )

  implicit def recipeCodec: CodecJson[Recipe] = CodecJson(
    p => {
      val image: Json = p.media match {
        case i: Image => p.media.asInstanceOf[Image].asJson
        case Video    => Json()
      }

      ("note"     := p.note) ->:
      ("link"     := p.link) ->:
      ("id"       := p.id) ->:
      ("media"    := ("type" := p.media.`type`) ->: jEmptyObject) ->:
      ("metadata" := p.metadata) ->:
      image
    },
    hc => {
      for {
        note <- (hc --\ "note").as[Option[String]].map(_.getOrElse(""))
        link <- (hc --\ "link").as[URL]
        id   <- (hc --\ "id").as[String]
        med  <- Media(hc)
        meta <- (hc --\ "metadata").as[Json]
      } yield Recipe(note, link, id, med, meta)
    }
  )


  implicit def imageCodec: CodecJson[Image] = CodecJson(
    i =>
      ("image" :=
        ("original" :=
          ("url" := i.url) ->:
            ("width" := i.width) ->:
            ("height" := i.height) ->:
            jEmptyObject
          ) ->: jEmptyObject
        ) ->: jEmptyObject,
    hc =>
      for {
        url    <- (hc --\ "image" --\ "original" --\ "url").as[URL]
        width  <- (hc --\ "image" --\ "original" --\ "width").as[Int]
        height <- (hc --\ "image" --\ "original" --\ "height").as[Int]
      } yield Image(url, width, height)
  )

  implicit def boardCodec: CodecJson[Board] = CodecJson(
    b =>
      ("url"         := b.url)  ->:
      ("counts"      := b.counts) ->:
      ("id"          := b.id)   ->:
      ("description" := b.description)   ->:
      ("name"        := b.name)   ->:
      jEmptyObject,
    hc =>
      for {
        n <- (hc --\ "name").as[String]
        d <- (hc --\ "description").as[String]
        i <- (hc --\ "id").as[String]
        u <- (hc --\ "url").as[URL]
        c <- (hc --\ "counts").as[Counts]
      } yield Board(n, d, i, u, c)
  )

  implicit def countsCodec: CodecJson[Counts] = CodecJson(
    c => ("pins" := c.pins) ->: jEmptyObject,
    hc => (hc --\ "pins").as[Int].map(Counts)
  )

  implicit def mealPlanEncoder: EncodeJson[MealPlan] = EncodeJson(
    mp =>
      ("Sunday"    :=? mp.sunday)    ->?:
      ("Monday"    :=? mp.monday)    ->?:
      ("Tuesday"   :=? mp.tuesday)   ->?:
      ("Wednesday" :=? mp.wednesday) ->?:
      ("Thursday"  :=? mp.thursday)  ->?:
      ("Friday"    :=? mp.friday)    ->?:
      ("Saturday"  :=? mp.saturday)  ->?:
      jEmptyObject
  )

}