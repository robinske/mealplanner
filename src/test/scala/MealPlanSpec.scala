package me.krobinson.mealplan

import argonaut.Json

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import me.krobinson.mealplan.generators._

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

  describe("#mealPlanJson") {
    it("should return a full week's worth of pins given a list of Pins 7 or longer") {
      forAll(genPinList(7)) { pl =>
        val result = mealPlanJson(pl)
        val hc = result.hcursor

        (hc --\ "Sunday").success shouldBe defined
        (hc --\ "Monday").success shouldBe defined
        (hc --\ "Tuesday").success shouldBe defined
        (hc --\ "Wednesday").success shouldBe defined
        (hc --\ "Thursday").success shouldBe defined
        (hc --\ "Friday").success shouldBe defined
        (hc --\ "Saturday").success shouldBe defined
      }
    }

    it ("should return a partial json object give a list shorter than 7") {
      forAll(genPinList(4)) { pl =>
        val result = mealPlanJson(pl)

        val hc = result.hcursor

        (hc --\ "Sunday").success shouldBe defined
        (hc --\ "Monday").success shouldBe defined
        (hc --\ "Tuesday").success shouldBe defined
        (hc --\ "Wednesday").success shouldBe defined
        (hc --\ "Thursday").success shouldBe empty
        (hc --\ "Friday").success shouldBe empty
        (hc --\ "Saturday").success shouldBe empty
      }
    }

    it ("should return empty JSON if the input list is empty") {
      mealPlanJson(List.empty) shouldBe Json()
    }
  }

}
