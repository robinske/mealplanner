package me.krobinson.mealplan.model

case class ApiResponse[A](
  data: A,
  nextPage: Option[String]
)

