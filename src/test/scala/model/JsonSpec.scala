package me.krobinson.mealplan.model.json

import java.net.URL
import scalaz.syntax.id._
import argonaut._, Argonaut._

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import me.krobinson.mealplan.json.generators._

import me.krobinson.mealplan.model.{Counts, Board, Pin, ApiResponse}

class JsonSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("#errorJson") {
    it("should produce valid json with the provided message") {
      val res = errorJson("this is an error")
      res shouldBe Json("error" := "this is an error")
    }
  }

  describe("#urlCodec") {
    it("should encode and decode into the same object") {
      forAll(genURL, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[URL])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardPinsCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoardPins, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[ApiResponse[List[Pin]]])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardMetaCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoardMeta, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[ApiResponse[Board]])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#pinCodec") {
    it("should encode and decode into the same object") {
      forAll(genPin, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Pin])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoard, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Board])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#countsCodec") {
    it("should encode and decode into the same object") {
      forAll(genCounts, minSuccessful(5), MaxSize(5)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Counts])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("mealPlanEncoder") {
    it("should encode the existing days") {
      forAll(genMealPlan, minSuccessful(5), MaxSize(5)) { orig =>
        val json = orig.asJson

        val hc = json.hcursor
        (hc --\ "Sunday").as[Pin].toOption shouldBe orig.sunday
        (hc --\ "Monday").as[Pin].toOption shouldBe orig.monday
        (hc --\ "Tuesday").as[Pin].toOption shouldBe orig.tuesday
        (hc --\ "Wednesday").as[Pin].toOption shouldBe orig.wednesday
        (hc --\ "Thursday").as[Pin].toOption shouldBe orig.thursday
        (hc --\ "Friday").as[Pin].toOption shouldBe orig.friday
        (hc --\ "Saturday").as[Pin].toOption shouldBe orig.saturday
      }
    }
  }
}
