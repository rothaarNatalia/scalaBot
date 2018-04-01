package bot2

import org.joda.time.DateTime

sealed trait Command {

  val commandName: String

  def execute: String = ???

}

case class CreatePoll(name: String, anonymous: Option[Boolean], visibility: Option[Visibility.Value], dateFrom: Option[DateTime], dateTo: Option[DateTime]) extends Command {
  override val commandName = "/create_poll"
}
case class DeletePoll(str: Long) extends Command {
  override val commandName = "/delete_poll"
}
case class StartPoll(str: Long) extends Command {
  override val commandName = "/start_poll"
}
case class StopPoll(str: Long) extends Command {
  override val commandName = "/stop_poll"
}
case object PollsList extends Command {
  override val  commandName = "/list"
}
case object Result extends Command {
  override val commandName = "/result"
}
case class Begin(str: Long) extends Command {
  override val commandName = "/begin"
}
case class AddQuestion(str: Long) extends Command {
  override val commandName = "/add_question"
}
case class DeleteQuestion(str: Long) extends Command {
  override val commandName = "/delete_question"
}
case class Answer(str: Long) extends Command {
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
