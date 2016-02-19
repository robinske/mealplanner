package me.krobinson.mealplan

import scalaj.http.{HttpOptions, HttpRequest, Http}
import argonaut._, Argonaut._

import me.krobinson.mealplan.model._
import me.krobinson.mealplan.model.json._

case class PinterestApiClient(at: AccessToken) {

  def buildPinterestApiRequest(path: String):HttpRequest = {
    val baseApi = "https://api.pinterest.com"
    Http(s"$baseApi/$path")
      .param("access_token", at.token)
      .param("limit", "100")
  }

  def request(path: String): HttpRequest = {
    buildPinterestApiRequest(path).option(HttpOptions.followRedirects(true))
  }

  def processResponse
  (req: HttpRequest, target: String)
  (decoder: DecodeJson[ApiResponse[List[Pin]]]): Result[List[Pin]] = {
    val resp = req.asString
    if (resp.isNotError) {
      resp.body.decodeOption[ApiResponse[List[Pin]]] match {
        case Some(ar) => Data(ar.data)
        case None     => Fail(s"Failed to decode JSON response body: ${resp.body}") // TODO - return meaningful parse errors
      }
    } else {
      Fail(
        s"""
          |Failed to fetch $target:
          |
          |${resp.body}
          |
          |Are you sure that's a valid Pinterest board?
        """.stripMargin
      )
    }
  }

//  def getBoardMetadata(board: String): Result[Board] = {
//    val req = request(s"v1/boards/$board")
//    processResponse(req, "board")(boardMetaCodec)
//  }

  def getBoardPins(board: String): Result[List[Pin]] = {
    /*
    getBoardMetadata(board) match {
      case Data(d) =>
        val numberOfPins = d.counts.pins // TODO - fetch more pages
      case Fail(m) => Fail(s"Failed to fetch board metadata: $m")
    }
    */

    val req = request(s"v1/boards/$board/pins")
    processResponse(req, "pins")(boardPinsCodec)
  }
}