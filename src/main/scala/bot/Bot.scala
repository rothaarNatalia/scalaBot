package bot

import bot.poll.PollManager
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{TelegramBot, Webhook}

object Bot extends TelegramBot with Webhook with Commands {

  def token = "549906144:AAFizyWrcNfep-fxlYT6CZuou1l-hU0rw4s"

  //override val port = 8443
  override val port = 8080
  override val webhookUrl = "https://15e40d17.ngrok.io"

  val parser = Parser
  val manager = PollManager

  onMessage { implicit msg =>
    reply({
      val cmd = parser.parseInput(msg.text.getOrElse(""))
      manager.execute(msg.from.flatMap(u => Some(u.id.toString)).getOrElse(""), cmd)
    })
    }


}

object App {

  def main(args: Array[String]): Unit = {
    Bot.run()
  }
}
