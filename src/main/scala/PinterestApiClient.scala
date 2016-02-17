package me.krobinson.mealplan

import dispatch._, Defaults._
import argonaut._, Argonaut._

import me.krobinson.mealplan.model.{ApiResponse, Pin}
import me.krobinson.mealplan.model.json._

case class PinterestApiClient(at: AccessToken) {

  val baseApi = "https://api.pinterest.com"

  def buildApiRequestUrl(path: String): Req = url(s"$baseApi/$path?access_token=${at.token}")

  def requestWithRedirect(path: String): Req = {
    val req = buildApiRequestUrl(path)

    Http.configure(_ setFollowRedirects true)(req OK as.String)
    val res = Http(req > (x => x))
    val redirectUrl = res().getHeader("Location")

    url(redirectUrl)
  }

  def getBoardPins(board: String): List[Pin] = {
    val req = requestWithRedirect(s"v1/boards/$board/pins")
    val resp = Http(req OK as.String)
    resp().decode[ApiResponse].map(_.data).getOrElse(List.empty)
  }
}