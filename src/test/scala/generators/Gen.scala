package me.krobinson.mealplan.json.generators

import java.net.URL

import me.krobinson.mealplan.MealPlan
import me.krobinson.mealplan.model.{Counts, Board, ApiResponse, Recipe}
import org.scalacheck.Gen, Gen._

object `package` {

  def nonEmptyAlphaStr: Gen[String] =
    alphaStr.flatMap(a => alphaChar map (c => a + c))

  def genURL: Gen[URL] =
    for {
      protocol <- oneOf("http", "https")
      host     <- nonEmptyAlphaStr
      tld      <- oneOf("io", "com", "net")
    } yield new URL(s"$protocol://www.$host.$tld")

  def genPin: Gen[Recipe] =
    for {
      note <- alphaStr
      link <- genURL
      id   <- posNum[Int].map(_.toString)
    } yield Recipe(note, link, id)

  def genPinList(size: Int) = Gen.listOfN(size, genPin)

  def genBoardPins: Gen[ApiResponse[List[Recipe]]] =
    for {
      nextPage <- option(genURL.map(_.toString))
      size     <- choose(1,5)
      pins     <- genPinList(size)
    } yield ApiResponse(pins, nextPage)

  def genCounts: Gen[Counts] =
    for {
      pins <- posNum[Int]
    } yield Counts(pins)

  def genBoard: Gen[Board] =
    for {
      id     <- posNum[Int].map(_.toString)
      name   <- nonEmptyAlphaStr
      desc   <- alphaStr
      url    <- genURL
      counts <- genCounts
    } yield Board(name, desc, id, url, counts)

  def genBoardMeta: Gen[ApiResponse[Board]] =
    for {
      nextPage <- option(genURL.map(_.toString))
      board    <- genBoard
    } yield ApiResponse(board, nextPage)

  def genMealPlan: Gen[MealPlan] =
    for {
      su <- option(genPin)
      mo <- option(genPin)
      tu <- option(genPin)
      we <- option(genPin)
      th <- option(genPin)
      fr <- option(genPin)
      sa <- option(genPin)
    } yield MealPlan(su, mo, tu, we, th, fr, sa)
}