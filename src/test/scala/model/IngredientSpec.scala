package me.krobinson.mealplan.model

import org.scalatest.{Matchers, FunSpec}

import scalaz.{-\/, \/-}
import me.krobinson.mealplan.model.Measurement._

class IngredientSpec extends FunSpec with Matchers {

  // TODO add test tables ?

  describe("Measurement#parseAmount") {
    it("can parse '2'") {
      parseAmount("2") shouldBe \/-(2.0)
    }

    it("can parse '2.75'") {
      parseAmount("2.75") shouldBe \/-(2.75)
    }

    it("can parse '1/2'") {
      parseAmount("1/2") shouldBe \/-(0.5)
    }

    it("can parse '2 1/2'") {
      parseAmount("2 1/2") shouldBe \/-(2.5)
    }

    it("fails to parse '1 (15 oz)'") {
      parseAmount("1 (15 oz)") shouldBe -\/("Unsupported parse value at this time: 1 (15 oz)")
    }
  }

  describe("Measurement#apply") {
    it("returns a valid measurement for '1/2 cup'") {
      Measurement("1/2 cup") shouldBe Cup(0.5)
    }

    it("returns a valid measurement for '3 cups'") {
      Measurement("3 cups") shouldBe Cup(3)
    }

    it("returns a valid measurement for '12 oz'") {
      Measurement("12 oz") shouldBe Ounce(12.0)
    }

    it("returns a valid measurement for '2 1/2 tbsp'") {
      Measurement("2 1/2 tbsp") shouldBe Tbsp(2.5)
    }

    it("returns a valid measurement for '2 1/2 TbSp' (case insensitive)") {
      Measurement("2 1/2 TbSp") shouldBe Tbsp(2.5)
    }

    it("returns a valid measurement for '8'") {
      Measurement("8") shouldBe Count(8)
    }

    it("fails to parse non numeric input") {
      Measurement("one") shouldBe OtherMeasurement("one")
    }

    it("fails to parse an unknown measurement unit") {
      Measurement("1 kilo") shouldBe OtherMeasurement("1 kilo")
    }

    it("fails to parse an empty input") {
      Measurement(" ") shouldBe OtherMeasurement(" ")
    }
  }

  describe("Measurement#parseCanAmount") {
    it("returns a valid measurement for '1 15 oz.'") {
      parseCanAmount("1 15 oz.") shouldBe \/-(Can(1, Ounce(15)))
    }

    it("returns a valid measurement for '100000 15 oz.'") {
      parseCanAmount("100000 15 oz.") shouldBe \/-(Can(100000, Ounce(15)))
    }

    it("returns a valid measurement for '1 (15 oz.)'") {
      parseCanAmount("1 (15 oz.)") shouldBe \/-(Can(1, Ounce(15)))
    }

    it("returns a valid measurement for '1 15 oz'") {
      parseCanAmount("1 15 oz") shouldBe \/-(Can(1, Ounce(15)))
    }

    it("returns a valid measurement for '1 (15000 oz)'") {
      parseCanAmount("1 (15000 oz)") shouldBe \/-(Can(1, Ounce(15000)))
    }
  }
}
