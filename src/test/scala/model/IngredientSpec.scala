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
      Measurement("one") shouldBe OtherMeasurement(List("one"))
    }

    it("fails to parse an unknown measurement unit") {
      Measurement("1 kilo") shouldBe OtherMeasurement(List("1 kilo"))
    }

    it("fails to parse an empty input") {
      Measurement(" ") shouldBe OtherMeasurement(List(" "))
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

  describe("Can+Can") {
    it("should return the expected number/size of cans when adding two of different sizes") {
      Can(1, Ounce(28)) + Can(1, Ounce(15)) shouldBe Can(3, Ounce(15))
    }

    it("should add just the number if the sizes match") {
      Can(1, Ounce(15)) + Can(4, Ounce(15)) shouldBe Can(5, Ounce(15))
    }
  }

  /*
  object Ingredient {

    def combineByName(l: List[Ingredient]): Map[(String, String), List[Ingredient]] =
      l.map { i =>
        val normalizedName = i.name.takeWhile(_ != ",")
        i.copy(name = normalizedName)
      } groupBy { g => (g.name, g.category) }

    def combineByMeasurement(l: List[Ingredient]): Map[String, List[Measurement]] =
      l.groupBy(_.measurement.classType).map { case (k,v) => (k, v.map(_.measurement)) }

    def reduceIngredients(l: List[Ingredient]): List[Ingredient] = {
      val grouped = combineByName(l).mapValues(combineByMeasurement)
      val reduced = grouped flatMap { case ((n,c),v) =>
        val measurements = v.map { case (a,m) => m.reduce(_ + _) }
        measurements map { m => Ingredient(n,c,m) }
      }
      reduced.toList
    }

  }
  */

  describe("Ingredient#combineByName") {
    it("should normalize and group similar ingredients") {
      val input = List(
        Ingredient("cilantro, fresh", "Produce", Count(1)),
        Ingredient("Cilantro, fresh leaves", "Produce", Count(3)),
        Ingredient("cilantro, dried", "Baking&Spices", Count(1))
      )

      val expected = Map(
        (("cilantro", "Produce"), List(Ingredient("cilantro", "Produce", Count(1)), Ingredient("cilantro", "Produce", Count(3)))),
        (("cilantro", "Baking&Spices"), List(Ingredient("cilantro", "Baking&Spices", Count(1))))
      )

      Ingredient.combineByName(input) should contain theSameElementsAs expected
    }
  }

  describe("Ingredient#combineByMeasurement") {
    it("should group measurements by their type") {
      val input = List(
        Ingredient("flour", "Baking&Spices", Cup(1)),
        Ingredient("flour", "Baking&Spices", Cup(2)),
        Ingredient("flour", "Baking&Spices", Tbsp(1)),
        Ingredient("flour", "Baking&Spices", Tbsp(2)),
        Ingredient("flour", "Baking&Spices", Tsp(1))
      )

      val expected = Map(
        ("cup", List(Cup(1), Cup(2))),
        ("tbsp", List(Tbsp(1), Tbsp(2))),
        ("tsp", List(Tsp(1)))
      )

      Ingredient.combineByMeasurement(input) should contain theSameElementsAs expected
    }
  }

  describe("Ingredient#reduceIngredients") {
    it("should reduce ingredients and add values where it makes sense") {
      val input = List(
        Ingredient("cilantro, fresh", "Produce", Count(1)),
        Ingredient("Cilantro, fresh leaves", "Produce", Count(3)),
        Ingredient("cilantro, dried", "Baking&Spices", Count(1)),
        Ingredient("flour", "Baking&Spices", Cup(1)),
        Ingredient("flour", "Baking&Spices", Cup(2)),
        Ingredient("flour", "Baking&Spices", Tbsp(1)),
        Ingredient("flour", "Baking&Spices", Tbsp(2)),
        Ingredient("flour", "Baking&Spices", Tsp(1))
      )

      val expected =
        List(
          Ingredient("cilantro","Produce",Count(4.0)),
          Ingredient("flour","Baking&Spices",Cup(3.0)),
          Ingredient("flour","Baking&Spices",Tsp(1.0)),
          Ingredient("flour","Baking&Spices",Tbsp(3.0)),
          Ingredient("cilantro","Baking&Spices",Count(1.0))
        )

      Ingredient.reduceIngredients(input) should contain theSameElementsAs expected
    }
  }
}
