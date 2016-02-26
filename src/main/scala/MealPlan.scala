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
  sunday: Option[Recipe],
  monday: Option[Recipe],
  tuesday: Option[Recipe],
  wednesday: Option[Recipe],
  thursday: Option[Recipe],
  friday: Option[Recipe],
  saturday: Option[Recipe]
)


object MealPlan {

  implicit class RecipeExt(li: List[Recipe]) {
    def shoppingList: List[Ingredient] =
      Ingredient.reduceIngredients(li.flatMap(_.ingredients))
  }


  def loadConfig: Properties = {
    val config = new Properties()
    config.load(new FileInputStream("config.properties"))
    config
  }

  def parseDays(days: Option[String]): Int =
    days.flatMap(d => Try(d.toInt).toOption).getOrElse(7)

  def apply(url: String, days: Option[String]): Json = {
    val config = loadConfig
    val accessToken = Authenticate(config)

    generateMealPlan(accessToken, url) match {
      case \/-(p) => mealPlanJson(p, parseDays(days))
      case -\/(m) => errorJson(m)
    }
  }

  def parseUrl(url: String): Option[URL] = Try(new URL(url)).toOption

  def generateMealPlan(at: AccessToken, url: String): Result[List[Recipe]] = {
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

  def mealPlanJson(plan: List[Recipe], days: Int): Json = {
    val meals = Random.shuffle(plan).take(days)
    Json(
      "shopping_list" := meals.shoppingList,
      "meals"         := meals
    )
  }
}
