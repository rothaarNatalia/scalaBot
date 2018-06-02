package bot2

import bot2.polls.{Quiz, QuizType, Visibility}
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  import BotCommands._

  override def skipWhitespace = true

  override val whiteSpace = "[ \t\r\f\n]+".r

  private def command: Parser[String] = "/[a-z_]+".r ^^ {
    _.toString
  }

  private def stringParam: Parser[String] = "[\\p{L}\\d!\\?-]+".r ^^ { _.toString }

  private def digitParam: Parser[Long] = "[0-9]+".r ^^ {
    _.toLong
  }

  private def escaping[T](p: Parser[T]): Parser[T] = "(" ~> p <~ ")"

  private def anonymous: Parser[Boolean] = ("yes" | "no") ^^ {
    case "yes" => true
    case "no" => false
  }

  private def visibility: Parser[Visibility.Value] = ("afterstop" | "continuous") ^^ {
    case "continuous" => Visibility.CONTINUOUS
    case "afterstop" => Visibility.AFTERSTOP
  }

  private def questionType: Parser[QuizType.Value] = ("open" | "choice" | "multi") ^^ {
    case "open" => QuizType.OPEN
    case "choice" => QuizType.CHOICE
    case "multi" => QuizType.MULTI
  }

  val dtFormat: String = "HH:mm:ss yy:MM:dd"
  val frm = DateTimeFormat.forPattern(dtFormat)
  private def dateTime: Parser[DateTime] = "[0-2][0-9]:[0-5][0-9]:[0-5][0-9] [0-9][0-9]:[0-1][0-9]:[0-3][0-9]".r ^^ { case dt => (DateTime.parse(dt, frm))}

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

  private def begin = BotCommands.begin ~> escaping(digitParam) ^^ {case id => Begin(id)}

  private def end = BotCommands.end ^^ {case _ => End}

  private def view = BotCommands.view ^^ {case _ => View}

  private def addQuestion = add_question ~> escaping(stringParam) ~
                                            opt(escaping(questionType)) ~
                                            rep(stringParam) ^^ {
                            case quiz ~ qTypq ~ answers =>
                              AddQuestion(Quiz(qTypq, quiz, Vector.empty, answers))}

  private def deleteQuestion = delete_question ~> escaping(digitParam) ^^ {case id => DeleteQuestion(id)}

  import bot2.polls.Answer
/*  private def userAnswerInt = escaping((stringParam | digitParam)) ^^ {
                            case id: Int =>  id
                            case str: String => str
                            }

  private def userAnswerString = escaping(stringParam) ^^ {
    case id: Int =>  id
    case str: String => str
  }  */


  private def fake = escaping((digitParam).+) ^^ {
    case p => {
      val o = p
      println(o)
      Unknown
    }

  }

  private def answer = BotCommands.answer ~> escaping(digitParam) ~ escaping( digitParam.+ |
                                                                              digitParam |
                                                                              stringParam

                                                                            ) ^^ {

                                          case id ~ uAnswers => {

                                              uAnswers match {
                                                case aswIdSeq: Seq[Long] => UserAnswer(id, aswIdSeq)
                                                case aswId: Long => UserAnswer(id, aswId)
                                                case string: String => UserAnswer(id, string)
                                              }
                                          }
                                          case p => {
                                            val o = p
                                            println(o)
                                            Unknown
                                          }
                                       }

  private def pollsCommandsParser = (createPoll | pollsList |
                                     deletePoll | startPoll |
                                       stopPoll | result |
                                          begin | end |
                                           view | answer |
                                    addQuestion | deleteQuestion) ^^ { case cmd: Command => cmd }

  def parseInput(input: String): Command = {

    parse(pollsCommandsParser, input.toLowerCase).getOrElse(Unknown)

  }


  def main(args: Array[String]): Unit = {


    val l = parseInput(
      """
        |/answer
        |(0)
        |
        |(1 4 5 5 12)
        |
        |""".stripMargin)

    println(l)


    /*val p =
      parse(createPoll,
        """/create_poll (johnny)""".stripMargin)
    match {
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
    }
*/
    println(l)

    PollManager.execute("rothaar", l)
  }
}

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