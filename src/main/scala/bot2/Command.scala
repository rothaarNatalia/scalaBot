package bot2

import bot2.polls.{QuizType, Visibility}
import org.joda.time.DateTime

sealed trait Command 

case class CreatePoll(name: String, anonymous: Option[Boolean], visibility: Option[Visibility.Value], dateFrom: Option[DateTime], dateTo: Option[DateTime]) extends Command
case class DeletePoll(id: Long) extends Command
case class StartPoll(id: Long) extends Command
case class StopPoll(id: Long) extends Command
case object PollsList extends Command
case class Result(id: Long) extends Command
case class Begin(id: Long) extends Command
case class AddQuestion(quiz: String, questionType: Option[QuizType.Value], pAnswers: List[String]) extends Command
case class DeleteQuestion(id: Long) extends Command
case class UserAnswer(id: Long, a: polls.Answer[_]*) extends Command
case object View extends Command
case object End extends Command
case object Unknown extends Command
case object Incorrect extends Command

sealed trait BotCommands {

  val create_poll = "/create_poll"
  val list = "/list"
  val delete_poll = "/delete_poll"
  val start_poll = "/start_poll"
  val stop_poll = "/stop_poll"
  val showResult = "/result"
  val begin = "/begin"
  val end = "/end"
  val add_question = "/add_question"
  val delete_question = "/delete_question"
  val view = "/view"
  val answer = "/answer"
  val unknown = "unknown"

}

object BotCommands extends BotCommands