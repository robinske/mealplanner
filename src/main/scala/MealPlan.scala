package me.krobinson.mealplan

import java.io.FileInputStream
import java.net.URL
import java.util.Properties

import scala.util.{Random, Try}
import scalaz.{-\/, \/-}
import scalaz.syntax.id._

import argonaut._, Argonaut._

import me.krobinson.mealplan.model._
import me.krobinson.mealplan.model.json._

case class MealPlan(
  sunday: Option[Pin],
  monday: Option[Pin],
  tuesday: Option[Pin],
  wednesday: Option[Pin],
  thursday: Option[Pin],
  friday: Option[Pin],
  saturday: Option[Pin]
)

object MealPlan {

  def loadConfig: Properties = {
    val config = new Properties()
    config.load(new FileInputStream("config.properties"))
    config
  }

  def apply(url: String): Json = {
    val config = loadConfig
    val accessToken = Authenticate(config)

    generateMealPlan(accessToken, url) match {
      case \/-(p) => mealPlanJson(p)
      case -\/(m) => errorJson(m)
    }
  }

  def parseUrl(url: String): Option[URL] = Try(new URL(url)).toOption

  def generateMealPlan(at: AccessToken, url: String): Result[List[Pin]] = {
    val client = PinterestApiClient(at)
    val boardUrl = parseUrl(url)

    boardUrl match {
      case Some(u) =>
        val cleanPath = u.getPath.stripPrefix("/").stripSuffix("/")
        client.getBoardPins(cleanPath)
      case None =>
        "Invalid URL, please try again.".left
    }
  }

  def mealPlanJson(plan: List[Pin]): Json = {
    val mealPlan = Random.shuffle(plan)

    MealPlan(
      mealPlan.lift(0),
      mealPlan.lift(1),
      mealPlan.lift(2),
      mealPlan.lift(3),
      mealPlan.lift(4),
      mealPlan.lift(5),
      mealPlan.lift(6)
    ).asJson
  }
}
