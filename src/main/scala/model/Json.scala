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

  implicit def apiResponseCodec: CodecJson[ApiResponse] = CodecJson(
    ar =>
      ("data" := ar.data) ->:
        ("page" := ar.page) ->:
        jEmptyObject
    ,
    hc =>
      for {
        d <- (hc --\ "data").as[List[Pin]]
        p <- (hc --\ "page").as[Page]
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

  implicit def pageCodec: CodecJson[Page] = CodecJson(
    p =>
      ("cursor" := p.cursor) ->:
      ("next" := p.next) ->:
      jEmptyObject
    ,
    hc =>
      for {
        c <- (hc --\ "cursor").as[String]
        n <- (hc --\ "next").as[String]
      } yield Page(c, n)
  )

}