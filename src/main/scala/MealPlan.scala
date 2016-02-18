package me.krobinson.mealplan

import java.io.FileInputStream
import java.net.URL
import java.util.Properties

import scala.io.StdIn
import scala.util.{Random, Try}

import me.krobinson.mealplan.model._

object MealPlan {

  private def loadConfig: Properties = {
    val config = new Properties()
    config.load(new FileInputStream("config.properties"))
    config
  }

  def getUrlFromUser(at: AccessToken): Option[URL] = {
    val boardUrl: String = StdIn.readLine("Enter the board URL you wish to use to generate a meal plan: ")
    Try(new URL(boardUrl)).toOption
  }

  def generateMealPlan(at: AccessToken): Result[List[Pin]] = {
    val client = PinterestApiClient(at)
    val boardUrl = getUrlFromUser(at)

    boardUrl match {
      case Some(url) =>
        val cleanPath = url.getPath.stripPrefix("/").stripSuffix("/")
        client.getBoardPins(cleanPath)
      case None =>
        Fail("Invalid URL, please try again.")
    }
  }

  def displayPlan(plan: List[Pin]): Unit = {
    val WEEKDAYS = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

    Random.shuffle(plan)
      .take(7)
      .zip(WEEKDAYS)
      .foreach { case (pin, day) =>
        println()
        println(day)
        println(s"${pin.note}")
        println(s"find the recipe at ${pin.link}")
      }
  }

  def displayError(message: String): Unit = println(message)

  def main(args: Array[String]) = {
    val config = loadConfig
    val accessToken = Authenticate(config)
    val plan: Result[List[Pin]] = generateMealPlan(accessToken)

    plan match {
      case Data(p) => displayPlan(p)
      case Fail(m) => displayError(m)
    }
  }

}
