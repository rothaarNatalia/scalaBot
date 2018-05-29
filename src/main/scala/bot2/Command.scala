package bot2

import bot2.polls.{Quiz}
import org.joda.time.DateTime
import bot2.polls.Answer

sealed trait Command {

  val commandName: String

  def execute: String = ???

}

case class CreatePoll(name: String, anonymous: Option[Boolean], visibility: Option[Visibility.Value], dateFrom: Option[DateTime], dateTo: Option[DateTime]) extends Command {
  override val commandName = "/create_poll"
}

case class DeletePoll(id: Long) extends Command {
  override val commandName = "/delete_poll"
}
case class StartPoll(id: Long) extends Command {
  override val commandName = "/start_poll"
}
case class StopPoll(id: Long) extends Command {
  override val commandName = "/stop_poll"
}
case object PollsList extends Command {
  override val  commandName = "/list"
}
case class Result(id: Long) extends Command {
  override val commandName = "/result"
}
case class Begin(id: Long) extends Command {
  override val commandName = "/begin"
}
case class AddQuestion(q: Quiz) extends Command {
  override val commandName = "/add_question"
}
case class DeleteQuestion(id: Long) extends Command {
  override val commandName = "/delete_question"
}
case class UserAnswer(id: Long, a: polls.Answer[_]*) extends Command {
  override val commandName = "/answer"
}
case object View extends Command {
  override val commandName = "/view"
}
case object End extends Command {
  override val commandName = "/end"
}


case object Unknown extends Command {
  override val commandName = "unknown"
}
case object Incorrect extends Command {
  override val commandName = "incorrect"
}
