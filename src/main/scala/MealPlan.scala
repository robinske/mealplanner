package me.krobinson.mealplan

import java.io.FileInputStream
import java.net.URL
import java.util.Properties

import scala.util.{Random, Try}

import me.krobinson.mealplan.model._

object MealPlan {

  def apply(url: String): String = {
    val config = loadConfig
    val accessToken = Authenticate(config)

    println("authenticated")
    val plan: Result[List[Pin]] = generateMealPlan(accessToken, url)
    println("got result")
    println(plan)

    plan match {
      case Data(p) =>
        println("here")
        displayPlan(p)
      case Fail(m) =>
        println("failed??")
        m
    }
  }

  private def loadConfig: Properties = {
    val config = new Properties()
    config.load(new FileInputStream("config.properties"))
    config
  }

  def parseUrl(url: String): Option[URL] = Try(new URL(url)).toOption

  def generateMealPlan(at: AccessToken, url: String): Result[List[Pin]] = {
    val client = PinterestApiClient(at)
    val boardUrl = parseUrl(url)

    boardUrl match {
      case Some(url) =>
        val cleanPath = url.getPath.stripPrefix("/").stripSuffix("/")
        client.getBoardPins(cleanPath)
      case None =>
        Fail("Invalid URL, please try again.")
    }
  }

  def displayPlan(plan: List[Pin]): String = {
    println("DISPLAYING??")
    val WEEKDAYS = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

    Random.shuffle(plan)
      .take(7)
      .zip(WEEKDAYS)
      .map { case (pin, day) =>
        s"""
          |$day
          |${pin.note}
          |find the recipe at ${pin.link}
        """.stripMargin
      }
      .mkString("\n\n")
  }
}
