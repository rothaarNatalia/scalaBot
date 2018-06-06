package bot2

import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}

object Bot extends TelegramBot with Polling with Commands {

  def token = "TOKEN"

  val parser = Parser
  val manager = PollManager

  onCommand("coin", "flip") {
    implicit msg => reply({
      val cmd = parser.parseInput(msg.text.getOrElse(""))
      manager.execute(msg.from.flatMap(_.username).getOrElse(""), cmd)
    })
  }
}
