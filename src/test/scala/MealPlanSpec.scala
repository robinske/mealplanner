package me.krobinson.mealplan

import argonaut._, Argonaut._
import me.krobinson.mealplan.model.{Ingredient, Recipe}

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import me.krobinson.mealplan.model.json.generators._

import me.krobinson.mealplan.model.json._
import me.krobinson.mealplan.MealPlan._


class MealPlanSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("#loadConfig") {
    it("should return expected config values") {
      val config = loadConfig
      config.getProperty("APP_ID") should not be empty
      config.getProperty("APP_SECRET") should not be empty
      config.getProperty("ACCESS_TOKEN") should not be empty
    }
  }

  describe("#parseUrl") {
    it("should return Some(URL) for a valid URL string") {
      val url = parseUrl("https://www.google.com")
      url shouldBe defined
      url.map(_.getProtocol) shouldBe Some("https")
      url.map(_.getHost) shouldBe Some("www.google.com")
    }

    it("should return None for an invalid URL") {
      parseUrl("foobar") shouldBe None
    }
  }

  describe("#parseDays") {
    it("should default to all days if no param is passed in") {
      val days = parseDays(None)
      days shouldBe 7
    }

    it("should default to all days if an invalid param is passed in") {
      val days = parseDays(Some("foobar"))
      days shouldBe 7
    }

    it("should note the days to return if given a param") {
      val days = parseDays(Some("5"))
      days shouldBe 5
    }
  }

  describe("#mealPlanJson") {
    it("should return a full week's worth of pins given a list of Pins 7 or longer") {
      forAll(genPinList(7)) { pl =>
        val result = mealPlanJson(pl,7)
        val hc = result.hcursor

        (hc --\ "meals").as[List[Json]].map(_.length).toOption shouldBe Some(7)
      }
    }

    it ("should return a partial json object give a list shorter than 7") {
      forAll(genPinList(4)) { pl =>
        val result = mealPlanJson(pl,7)

        val hc = result.hcursor

        (hc --\ "meals").as[List[Json]].map(_.length).toOption shouldBe Some(4)
      }
    }

    it ("should return empty JSON if the input list is empty") {
      val expected = Json(
        "shopping_list" := List.empty[Ingredient],
        "meals" := List.empty[Recipe]
      )

      mealPlanJson(List.empty,7) shouldBe expected
    }

    it ("should return a partial list of JSON matching the number of requested days") {
      forAll(genPinList(7)) { pl =>
        val result = mealPlanJson(pl,5)

        val hc = result.hcursor

        (hc --\ "meals").as[List[Json]].map(_.length).toOption shouldBe Some(5)
      }
    }
  }

}
