package me.krobinson.mealplan.model.json.generators

import java.net.URL

import argonaut._, Argonaut._
import me.krobinson.mealplan.MealPlan
import me.krobinson.mealplan.model._
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

  def genImage: Gen[Image] =
    for {
      url <- genURL
      w   <- posNum[Int]
      h   <- posNum[Int]
    } yield Image(url, w, h)

  def genRecipe: Gen[Recipe] =
    for {
      note    <- alphaStr
      link    <- genURL
      id      <- posNum[Int].map(_.toString)
      medKey  <- alphaStr
      img     <- genImage
      metaKey <- nonEmptyAlphaStr
      metaVal <- alphaStr
    } yield Recipe(None, note, link, id, img, Json(metaKey := metaVal))

  def genPinList(size: Int) = Gen.listOfN(size, genRecipe)

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
      su <- option(genRecipe)
      mo <- option(genRecipe)
      tu <- option(genRecipe)
      we <- option(genRecipe)
      th <- option(genRecipe)
      fr <- option(genRecipe)
      sa <- option(genRecipe)
    } yield MealPlan(su, mo, tu, we, th, fr, sa)
}