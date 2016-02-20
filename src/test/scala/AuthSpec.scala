package me.krobinson.mealplan

import org.scalatest.{Matchers, FunSpec}

class AuthSpec extends FunSpec with Matchers {

  describe("Authorization#apply") {
    it("should return an access token") {
      val props = MealPlan.loadConfig
      assert(Authenticate(props).isInstanceOf[AccessToken])
    }
  }

}