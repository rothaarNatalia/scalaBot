package bot2

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import play.api.libs.json.Json

/**
  * Created by rothaar on 06.02.2018.
  */
case class Handler() {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future map/flatmap in the end
  implicit val executionContext = system.dispatcher

  val conf = ConfigFactory.load()

  val token = conf.getString("token")
  val url = conf.getString("url")

  //private def jsonParser =


  def requestHandler(rq: HttpRequest): HttpResponse = {

    import Parser._

    rq match {
    case HttpRequest(POST, Uri.Path("/rothaar_bot"), _, _, _) =>{

      val body = Json.parse(rq.entity.toString) toString()

      val cmd = parseInput(body)

      val response = cmd.execute

      HttpResponse(
        entity = HttpEntity(
          ContentTypes.`application/json`,
          "{\"bot\": \"Hello world!\"}"))

    }

    case HttpRequest(GET, Uri.Path("/rothaar_bot"), _, _, _) =>
      HttpResponse(entity = HttpEntity(
        ContentTypes.`text/html(UTF-8)`,
        "<html><body>Hello world!</body></html>"))

    case r: HttpRequest =>
      r.discardEntityBytes() // important to drain incoming HTTP Entity stream
      HttpResponse(404, entity = "Unknown resource!")
  }

  }

  val bindingFuture = Http().bindAndHandleSync(requestHandler, "localhost", 80)

}
