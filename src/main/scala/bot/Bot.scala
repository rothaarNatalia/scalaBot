package bot

import java.util.Properties

import bot.commands.{Command, UnknownCommand}


import scala.io.StdIn

/**
  * Created by rothaar on 04.02.2018.
  */
object Bot{

  private val token = "549906144:AAFizyWrcNfep-fxlYT6CZuou1l-hU0rw4s"
  private val endPoint = "https://api.telegram.org/bot"

  def main(args: Array[String]) {

    Handler().bindingFuture

    println(s"Server online at http://localhost:88/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    /*bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done*/

  }
}
