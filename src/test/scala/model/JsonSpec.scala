package me.krobinson.mealplan.model.json

import java.net.URL
import scala.io.Source
import scalaz.\/-
import scalaz.syntax.id._
import argonaut._, Argonaut._

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import me.krobinson.mealplan.model.json.generators._

import me.krobinson.mealplan.model._

class JsonSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("#errorJson") {
    it("should produce valid json with the provided message") {
      val res = errorJson("this is an error")
      res shouldBe Json("error" := "this is an error")
    }
  }

  describe("#urlCodec") {
    it("should encode and decode into the same object") {
      forAll(genURL, minSuccessful(3), MaxSize(3)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[URL])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardPinsCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoardPins, minSuccessful(3), MaxSize(3)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[ApiResponse[List[Recipe]]])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardMetaCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoardMeta, minSuccessful(3), MaxSize(3)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[ApiResponse[Board]])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#imageCodec") {
    it("should encode and decode into the same object") {
      forAll(genImage, minSuccessful(3), MaxSize(3)) { orig: Image =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Image])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#recipeCodec") {
    it("should encode and decode into the same object") {
      forAll(genRecipe, minSuccessful(3), MaxSize(3)) { orig: Recipe =>
        val jString = orig.asJson(recipeCodec).nospaces
        val res = handleJson(jString.decode[Recipe])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#boardCodec") {
    it("should encode and decode into the same object") {
      forAll(genBoard, minSuccessful(3), MaxSize(3)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Board])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#countsCodec") {
    it("should encode and decode into the same object") {
      forAll(genCounts, minSuccessful(3), MaxSize(3)) { orig =>
        val jString = orig.asJson.nospaces
        val res = handleJson(jString.decode[Counts])(identity)
        res shouldBe orig.right
      }
    }
  }

  describe("#ingredentCodec") {
    it("should decode the ingredients nested in the meta fields") {
      val json = Source.fromFile("src/test/resources/ingredients.json").getLines().mkString

      val res = handleJson(json.decode[ApiResponse[List[Recipe]]])(_.data)

      val expected = List(
        Ingredient("Strip loin steak, Grass Fed", "Meat", Lb(1)),
        Ingredient("Basil, dried", "Produce", Tsp(2)),
        Ingredient("Chives, dried", "Produce", Tsp(3)),
        Ingredient("Dill, dried", "Produce", Tsp(2)),
        Ingredient("Garlic powder", "Produce", Tsp(2)),
        Ingredient("Greens, mixed", "Produce", Cup(3)),
        Ingredient("Kale", "Produce", Cup(3)),
        Ingredient("Parsley, dried", "Produce", Tsp(2)),
        Ingredient("Coconut milk, light", "Canned Goods", Tbsp(2)),
        Ingredient("Mayo, whole", "Condiments", Tbsp(2)),
        Ingredient("Black pepper", "Baking & Spices", Tsp(0.5)),
        Ingredient("Sea salt", "Baking & Spices", Tsp(1)),
        Ingredient("Ghee", "Dairy", Tbsp(1))
      )

      res.map(_.flatMap(_.ingredients)) shouldBe \/-(expected)
    }
  }
}
