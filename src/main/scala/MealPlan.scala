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

  def loadConfig: Properties = {
    val config = new Properties()
    config.load(new FileInputStream("config.properties"))
    config
  }

  def parseDays(days: Option[String]): Days = {
    days match {
      case None => (1,1,1,1,1,1,1)
      case Some(d) =>
        val l = d.split(",").map(_.toLowerCase)
        def setBit(day: String): Int = if (l.find(_ == day).isDefined) 1 else 0
        (
          setBit("sunday"),
          setBit("monday"),
          setBit("tuesday"),
          setBit("wednesday"),
          setBit("thursday"),
          setBit("friday"),
          setBit("saturday")
        )
    }
  }

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

  def mealPlanJson(plan: List[Recipe], days: Days): Json = {
    val mealPlan = Random.shuffle(plan)

    def addMeal(idx: Int, day: Int): Option[Recipe] =
      if (day == 1) mealPlan.lift(idx) else None

    MealPlan(
      addMeal(0, days._1),
      addMeal(1, days._2),
      addMeal(2, days._3),
      addMeal(3, days._4),
      addMeal(4, days._5),
      addMeal(5, days._6),
      addMeal(6, days._7)
    ).asJson
  }
}
