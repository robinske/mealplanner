package me.krobinson.mealplan

import scalaj.http.{HttpOptions, HttpRequest, Http}
import argonaut._, Argonaut._

import me.krobinson.mealplan.model._
import me.krobinson.mealplan.model.json._

import scalaz.{-\/, \/-}
import scalaz.syntax.id._

case class PinterestApiClient(at: AccessToken) {

  def buildPinterestApiRequest(path: String): HttpRequest = {
    val baseApi = "https://api.pinterest.com"
    Http(s"$baseApi/$path")
      .param("access_token", at.token)
      .param("limit", "100")
  }

  def request(path: String, params: Seq[(String, String)] = List.empty): HttpRequest = {
    buildPinterestApiRequest(path)
      .params(params)
      .option(HttpOptions.followRedirects(true))
  }

  def processResponse[A]
  (req: HttpRequest, target: String)
  (decoder: DecodeJson[ApiResponse[A]]): Result[A] = {
    val resp = req.asString
    if (resp.isNotError) {
      handleJson(resp.body.decode[ApiResponse[A]](decoder))(_.data)
    } else {
        s"""Failed to fetch $target:
          |
          |${resp.body}
          |
          |Are you sure that's a valid Pinterest board?
        """.stripMargin.left
    }
  }

  def getBoardMetadata(board: String): Result[Board] = {
    val req = request(s"v1/boards/$board", Seq(("fields", "url,id,name,counts,description")))
    processResponse(req, "board")(boardMetaCodec)
  }

  def getBoardPins(board: String): Result[List[Recipe]] = {
    // TODO use this to fetch more pins
    val numReqs = getBoardMetadata(board) match {
      case \/-(d) if d.counts.pins > 100 => Math.min(2, d.counts.pins / 100)
      case _ => 0
    }

    val req = request(s"v1/boards/$board/pins")
    processResponse[List[Recipe]](req, "pins")(boardPinsCodec)
  }
}