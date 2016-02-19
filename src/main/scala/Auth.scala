package me.krobinson.mealplan

import java.util.{UUID, Properties}

import scalaj.http.{HttpOptions, Http}

case class AppAuth(appId: String, appSecret: String)

object AppAuth {
  def apply(p: Properties): AppAuth = {
    val appId = p.getProperty("APP_ID")
    val appSecret = p.getProperty("APP_SECRET")
    AppAuth(appId, appSecret)
  }
}

case class AccessToken(token: String) extends AnyVal

object Authenticate {
  // TODO this should actually do oauth...
  def apply(p: Properties): AccessToken = {
    val state = UUID.randomUUID().toString
    val req = Http("https://api.pinterest.com/oauth")
      .options(HttpOptions.followRedirects(true))
      .param("response_type", "code")
      .param("client_id", p.getProperty("APP_ID"))
      .param("state", state)
      .param("scope", "read_public")
      .param("redirect_uri", "https://localhost")

    val resp = req.asString

    AccessToken(p.getProperty("ACCESS_TOKEN"))
  }
}