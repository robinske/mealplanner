package me.krobinson.mealplan

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source
import scalaj.http.{HttpResponse, HttpRequest, HttpOptions}

import me.krobinson.mealplan.model.json._

class PinterestApiClientSpec extends FunSpec with Matchers with MockitoSugar {

  val client = PinterestApiClient(AccessToken("fake_access_token"))

  describe("#buildPinterestApiRequest") {
    it("should construct the request with the expected params") {
      val req = client.buildPinterestApiRequest("")

      val expectedParams = Seq(
        ("access_token", "fake_access_token"),
        ("limit", "100")
      )

      req.params should contain theSameElementsAs expectedParams
    }

    it("should construct the URL with the expected path") {
      val req = client.buildPinterestApiRequest("my/path/here")
      req.url shouldBe "https://api.pinterest.com/my/path/here"
    }
  }

  describe("#request") {
    it("should add additional params to the request object if provided") {
      val req = client.request("path", Seq(("bar", "baz"), ("foo", "bar")))
      val expectedParams = Seq(
        ("access_token", "fake_access_token"),
        ("limit", "100"),
        ("foo", "bar"),
        ("bar", "baz")
      )
      req.params should contain theSameElementsAs expectedParams
    }

    it("should use the default params if not provided any additional") {
      val req = client.request("path")
      val expectedParams = Seq(
        ("access_token", "fake_access_token"),
        ("limit", "100")
      )
      req.params should contain theSameElementsAs expectedParams
    }

    /*
    it("should be configured to follow redirects") {
      val req = client.request("path")
      println(req.options.toList)
      req.options should contain (HttpOptions.followRedirects(true))
    }
    */
  }

  describe("#processResponse") {
    it("should decode the response json for a valid request") {
      val req = mock[HttpRequest]
      val resp = mock[HttpResponse[String]]

      val validJson = Source.fromFile("src/test/resources/boardpins.json").getLines().mkString

      when(resp.body).thenReturn(validJson)
      when(resp.isNotError).thenReturn(true)
      when(req.asString).thenReturn(resp)

      val result = client.processResponse(req, "pins")(boardPinsCodec)
      val pins = result.getOrElse(List.empty)
      pins should not be empty
      assert(pins.length == 25)
    }

    it("should return the expected failure for a non-decodeable response") {
      val req = mock[HttpRequest]
      val resp = mock[HttpResponse[String]]

      val validJson = Source.fromFile("src/test/resources/boardpins.json").getLines().mkString

      when(resp.body).thenReturn(validJson)
      when(resp.isNotError).thenReturn(true)
      when(req.asString).thenReturn(resp)

      val result = client.processResponse(req, "pins")(boardMetaCodec)
      assert(result.isLeft)
      result.leftMap[Unit] { l =>
        l should startWith ("Failure occured when decoding JSON response body")
      }
    }

    it("should return the expected failure for a non-parseable response") {
      val req = mock[HttpRequest]
      val resp = mock[HttpResponse[String]]

      when(resp.body).thenReturn("json")
      when(resp.isNotError).thenReturn(true)
      when(req.asString).thenReturn(resp)

      val result = client.processResponse(req, "pins")(boardMetaCodec)
      assert(result.isLeft)
      result.leftMap[Unit] { l =>
        l shouldBe "Invalid JSON response"
      }
    }

    it("should return the expected failure for a failed request") {
      val req = mock[HttpRequest]
      val resp = mock[HttpResponse[String]]

      when(resp.isNotError).thenReturn(false)
      when(req.asString).thenReturn(resp)

      val result = client.processResponse(req, "pins")(boardMetaCodec)
      assert(result.isLeft)
      result.leftMap[Unit] { l =>
        l should startWith ("Failed to fetch pins:")
      }
    }
  }

}
