package bot

import bot.poll.{PollManager, QuizType, Visibility}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

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

  private def result = showResult ~> escaping(digitParam) ^^ { case id => Result(id) }

  private def begin = BotCommands.begin ~> escaping(digitParam) ^^ {case id => Begin(id)}

  private def end = BotCommands.end ^^ {case _ => End}

  private def view = BotCommands.view ^^ {case _ => View}

  private def addQuestion = add_question ~> escaping(stringParam) ~
                                            opt(escaping(questionType)) ~
                                            rep(stringParam) ^^ {
                            case quiz ~ qTypq ~ answers =>
                              AddQuestion(quiz, qTypq, answers)}

  private def deleteQuestion = delete_question ~> escaping(digitParam) ^^ {case id => DeleteQuestion(id)}

  private def answer = BotCommands.answer ~> escaping(digitParam) ~ escaping( digitParam.+ |
                                                                              stringParam

                                                                            ) ^^ {

                                          case id ~ uAnswers => {

                                              uAnswers match {
                                                case aswIdSeq: List[Long] => UserAnswer(id, aswIdSeq)
                                                case string: String => UserAnswer(id, string)
                                              }
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

    val manager = PollManager

    val l1 = parseInput(
      s"""
         |/list|
         |""".stripMargin)

    println(l1)

    val rslL = manager.execute("rothaar", l1)

    println(rslL)

    val c = parseInput(
      """
        |/create_poll
        |(johnny)
        |(yes)
        |(continuous)
        |
        |""".stripMargin)

    val rslC = manager.execute("rothaar", c)

    val pId =rslC

    val b = parseInput(
      s"""
        |/begin ($rslC)
        |""".stripMargin)

    val rslB = manager.execute("rothaar", b)



    val aq = parseInput(
      """
        |/add_question
        |(quiz?)
        |(choice)
        |eins
        |zwei
        |drei
        |""".stripMargin)

    val rslAq = manager.execute("rothaar", aq)

    val qId = rslAq

    val fId = manager.execute("rothaar_fake", c)
    println(manager.execute("rothaar_fake", c))

    val sP = parseInput(
      s"""
        |/start_poll ($rslC)
        |""".stripMargin)

    val sPf = parseInput(
      s"""
         |/start_poll ($rslC)
         |""".stripMargin)

    val rslSpF = manager.execute("rothaar_fake", sP)
    println(manager.execute("rothaar_fake", sPf))


    val rslSp = manager.execute("rothaar", sP)


    println(manager.execute("rothaar_fake", parseInput(
      s"""
         |/begin ($fId)
         |""".stripMargin)))

    val fqId = manager.execute("rothaar_fake", aq)

    println(manager.execute("rothaar_fake", parseInput(
      """
        |/add_question
        |(quiz?)
        |(choice)
        |eins
        |zwei
        |drei
        |""".stripMargin)))


    println("###########3 choice answer")
    val l = parseInput(
      s"""
        |/answer
        |($qId)
        |
        |(1)
        |
        |""".stripMargin)

    println(l)

    val rslA = manager.execute("rothaar", l)

    println(rslA)

    println("Multi one answ")
    println(manager.execute("rothaar_fake", parseInput(
      s"""
         |/answer
         |($fqId)
         |
        |(1)
         |
        |""".stripMargin)))


    val rslL2 = manager.execute("rothaar", l1)

    println(rslL2)


    println(manager.execute("rothaar_fake", l1))

    val r = parseInput(
      s"""
         |/result ($rslC)
        |""".stripMargin)


    println(manager.execute("rothaar", r))


    println("result fake")
    println(manager.execute("rothaar", parseInput(
      s"""
         |/result ($fId)
         |""".stripMargin)))

    println(manager.execute("rothaar_fake", parseInput(
      s"""
         |/result ($fId)
         |""".stripMargin)))

    val v = parseInput(
      s"""
         |/view ($rslC)
         |""".stripMargin)

    println(v)

    println(manager.execute("rothaar", v))

    val dq = parseInput(
      s"""
         |/delete_question
         |($qId)
         |
        |""".stripMargin)

    println(manager.execute("rothaar_fake", dq))
    println(manager.execute("rothaar", dq))

    println(manager.execute("rothaar", r))

    println(manager.execute("rothaar", v))

    val dP = parseInput(
      s"""
         |/delete_poll ($rslC)
         |""".stripMargin)

    println("dp rothaar_fake")
    println(manager.execute("rothaar_fake", dP))

    println(manager.execute("rothaar", dP))

    val stP = parseInput(
      s"""
         |/stop_poll ($rslC)
         |""".stripMargin)


    println(manager.execute("rothaar_fake", stP))

    println(manager.execute("rothaar", stP))

    println(manager.execute("rothaar", l))

    val e = parseInput(
      s"""
         |/end
         |""".stripMargin)


    println(manager.execute("rothaar", e))

    println(manager.execute("rothaar_fake", l1))

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
  }
}

