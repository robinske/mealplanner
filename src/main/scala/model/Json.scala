package me.krobinson.mealplan.model

import java.net.URL

import argonaut._, Argonaut._, DecodeResult._

import scala.util.{Success, Try}

package object json {
  implicit def urlCodec: CodecJson[URL] = CodecJson(
    url => url.toString.asJson,
    hc =>
      hc.as[String] flatMap { urlStr =>
        Try(new URL(urlStr)) match {
          case Success(u) => ok(u)
          case _          => fail("Failed to parse link as URL", hc.history)
        }
      }
  )

  implicit def boardPinsCodec: CodecJson[ApiResponse[List[Pin]]] = CodecJson(
    ar =>
      ("data" := ar.data) ->:
      ("nextPage" := ar.nextPage) ->:
      jEmptyObject,
    hc =>
      for {
        d <- (hc --\ "data").as[List[Pin]]
        p <- (hc --\ "page" --\ "next").as[Option[String]]
      } yield ApiResponse(d, p)
  )

  implicit def boardMetaCodec: CodecJson[ApiResponse[Board]] = CodecJson(
    ar =>
      ("data" := ar.data) ->:
      ("nextPage" := ar.nextPage) ->:
      jEmptyObject,
    hc =>
      for {
        d <- (hc --\ "data").as[Board]
        p <- (hc --\ "page" --\ "next").as[Option[String]]
      } yield ApiResponse(d, p)
  )

  implicit def pinCodec: CodecJson[Pin] = CodecJson(
    p =>
      ("note" := p.note) ->:
      ("link" := p.link) ->:
      ("id"   := p.id)   ->:
      jEmptyObject
    ,
    hc =>
      for {
        n <- (hc --\ "note").as[String]
        l <- (hc --\ "link").as[URL]
        i <- (hc --\ "id").as[String]
      } yield Pin(n, l, i)
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
        n <- (hc --\ "note").as[String]
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

}