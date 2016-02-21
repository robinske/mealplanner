package me.krobinson.mealplan

import argonaut.Json

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import me.krobinson.mealplan.json.generators._

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
  /*
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

   */

  describe("#parseDays") {
    it("should default to all days if no param is passed in") {
      val days = parseDays(None)
      days shouldBe (1,1,1,1,1,1,1)
    }

    it("should note the days to return if given a param") {
      val days = parseDays(Some("Tuesday,wednesday,FriDay"))
      days shouldBe (0,0,1,1,0,1,0)
    }
  }

  describe("#mealPlanJson") {
    it("should return a full week's worth of pins given a list of Pins 7 or longer") {
      forAll(genPinList(7)) { pl =>
        val result = mealPlanJson(pl,(1,1,1,1,1,1,1))
        val hc = result.hcursor

        assert((hc --\ "Sunday").succeeded)
        assert((hc --\ "Monday").succeeded)
        assert((hc --\ "Tuesday").succeeded)
        assert((hc --\ "Wednesday").succeeded)
        assert((hc --\ "Thursday").succeeded)
        assert((hc --\ "Friday").succeeded)
        assert((hc --\ "Saturday").succeeded)
      }
    }

    it ("should return a partial json object give a list shorter than 7") {
      forAll(genPinList(4)) { pl =>
        val result = mealPlanJson(pl,(1,1,1,1,1,1,1))

        val hc = result.hcursor

        assert((hc --\ "Sunday").succeeded)
        assert((hc --\ "Monday").succeeded)
        assert((hc --\ "Tuesday").succeeded)
        assert((hc --\ "Wednesday").succeeded)
        assert((hc --\ "Thursday").failed)
        assert((hc --\ "Friday").failed)
        assert((hc --\ "Saturday").failed)
      }
    }

    it ("should return empty JSON if the input list is empty") {
      mealPlanJson(List.empty,(1,1,1,1,1,1,1)) shouldBe Json()
    }

    it ("should return a partial list of JSON matching the requested days") {
      forAll(genPinList(7)) { pl =>
        val result = mealPlanJson(pl,(1,0,1,1,1,1,0))

        val hc = result.hcursor

        assert((hc --\ "Sunday").succeeded)
        assert((hc --\ "Monday").failed)
        assert((hc --\ "Tuesday").succeeded)
        assert((hc --\ "Wednesday").succeeded)
        assert((hc --\ "Thursday").succeeded)
        assert((hc --\ "Friday").succeeded)
        assert((hc --\ "Saturday").failed)
      }
    }
  }

}
