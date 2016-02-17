package me.krobinson.mealplan.model

case class Page(
  cursor: String,
  next: String
)

case class ApiResponse(
  data: List[Pin],
  page: Page
)

