package me.krobinson.mealplan

import java.util.Properties

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
  def apply(p: Properties): AccessToken =
    AccessToken(p.getProperty("ACCESS_TOKEN"))
}