package bot2

import org.joda.time.DateTime

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  import BotCommands._

  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f\n]+".r


  private def command: Parser[String] = "/[a-z_]+".r ^^ {
    _.toString
  }

  private def stringParam: Parser[String] = "[\\p{Alpha}]+".r ^^ {
    _.toString
  }

  private def digitParam: Parser[Long] = "[0-9]+".r ^^ {
    _.toLong
  }

  private def escaping[T](p: Parser[T]): Parser[T] = "(" ~> p <~ ")"

  private def anonymous: Parser[Boolean] = ("yes" | "no") ^^ {
    case "yes" => true
    case "no" => false
  }

  private def visibility: Parser[Visibility.Value] = ("afterstop" | "continuous") ^^ {
    case "continuous" => Visibility.CONTINIOUS
    case "afterstop" => Visibility.AFTERSTOP
  }

  private def dateTime: Parser[DateTime] = ("hh:mm:ss yy:MM:dd").r ^^ {
    (DateTime.parse(_))
  }

  private def createPoll = create_poll ~> escaping(stringParam) ~ opt(escaping(anonymous)) ~
    opt(escaping(visibility)) ~
    opt(escaping(dateTime)) ~
    opt(escaping(dateTime)) ^^ {
    case name ~ anonymous ~ visibility ~ fromDate ~ toDate =>
      CreatePoll(name, anonymous, visibility, fromDate, toDate)
  }

  private def pollsList = list ^^ { case list => PollsList }

  private def deletePoll = delete_poll ~> escaping(digitParam) ^^ { case id => DeletePoll(id) }

  private def startPoll = start_poll ~> escaping(digitParam) ^^ { case id => StartPoll(id) }

  private def stopPoll = stop_poll ~> escaping(digitParam) ^^ { case id => StopPoll(id) }

  private def result = showResult ^^ { case _ => Result }

  private def pollsCommandsParser = (createPoll | pollsList |
                                     deletePoll | startPoll |
                                       stopPoll | result) ^^ { case cmd: Command => cmd }

  def parseInput(input: String): Command = {

    parse(pollsCommandsParser, input).getOrElse(Unknown)

  }


  def main(args: Array[String]) = {

    val l = parseInput(
      """
        |
        |
        |           /start_poll
        |
        |
        |(54643513)
        |(johnny)""".stripMargin)

    println(l)


    val p =
      parse(createPoll,
        """/create_poll (johnny)""".stripMargin)
    /*match {
        case Success(matched,_) => matched
        case Failure(msg,_) => println("FAILURE: " + msg)
        case Error(msg,_) => println("ERROR: " + msg)
      }

    val p =
    parse(parser,
      """/create_poll
        |(johnny)
        |(command)
        |(ntgjl)""".stripMargin) match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("FAILURE: " + msg)
      case Error(msg,_) => println("ERROR: " + msg)
    }*/

    println(p)
  }
}

sealed trait BotCommands {

  val create_poll = "/create_poll"
  val list = "/list"
  val delete_poll = "/delete_poll"
  val start_poll = "/start_poll"
  val stop_poll = "/stop_poll"
  val showResult = "/result"
  val unknown = "unknown"

}

object BotCommands extends BotCommands