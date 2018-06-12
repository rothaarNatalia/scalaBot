package bot

import bot.poll.PollManager
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}

object Bot extends TelegramBot with Polling with Commands {

  def token = ""

  val parser = Parser
  val manager = PollManager

  onCommand("/create_poll", "/list", "/delete_poll",
            "/start_poll", "/stop_poll", "/result",
            "/begin", "/end", "/add_question",
            "/delete_question", "/view", "/answer") {

    implicit msg => reply({
      val cmd = parser.parseInput(msg.text.getOrElse(""))
      manager.execute(msg.from.flatMap(_.username).getOrElse(""), cmd)
    })
  }
}

object App {

  def main(args: Array[String]): Unit = {
    Bot.run()
  }
}
