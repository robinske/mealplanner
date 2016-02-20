package me.krobinson.mealplan

import com.twitter.finagle.{Service, Http}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.server.TwitterServer
import com.twitter.util.Await
import io.finch._

object Main extends TwitterServer {

  val getHealth: Endpoint[String] = get("health") { Ok("Hello, World!") } // TODO ascii art. obviously.
  val getMealPlan: Endpoint[String] = {
    get("mealplan" :: param("boardUrl")) { boardUrl: String =>
      Ok(MealPlan(boardUrl).nospaces)
    }
  }

  val api: Service[Request, Response] = (getHealth :+: getMealPlan).toService
  val server = Http.server.serve(":8080", api)

  onExit { server.close() }

  Await.ready(adminHttpServer)

}