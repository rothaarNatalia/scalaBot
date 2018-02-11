package bot.votes

import play.api.libs.json.JsResult.Exception

import scala.collection.immutable.HashMap

abstract class Quiz{

  protected var usersAnswers: List[T]

  def createQuiz(quizType: QuizType.Value, question: String, answers: Option[HashMap[Int, String]]): Quiz[T] = {

    quizType match {
      case QuizType.CHOICE => if (answers == None) throw IllegalArgumentException
                                    else ChoiceQuiz(question, answers.get)
      case QuizType.MULTI => if (answers == None) throw IllegalArgumentException
                                      else MultiQuiz(question, answers.get)
      case QuizType.OPEN =>  OpenQuiz(question)
    }
  }

  def vote

  def result
}

case class MultiQuiz[Int](question: String, answers: HashMap[Int, String]) extends Quiz[Int]{

  final private val quiz = question

  final private val possibleAnswers: HashMap[Int, String] = answers

  var usersAnswers = List[Int]()

  final override def result: Map[String, Int] = {

    val result = usersAnswers map ((_, 1)) groupBy (_._1) map (v => (v._1, v._2 reduce(_._2 + _._2)))

    result zip possibleAnswers map (v => (v._2._2, v._1._2))
  }

  final override def vote(answer: Seq[Int]): Unit = {

    answer foreach (a =>
              if (possibleAnswers.contains(a))
                usersAnswers += a
              else throw IllegalArgumentException
      )
  }
}

case class ChoiceQuiz(question: String, answers: HashMap[Int, String]) extends Quiz[Int]{

  final private val quiz = question

  final private val possibleAnswers: HashMap[Int, String] = answers

  var usersAnswers = List()

  final override def result: Map[String, Int] = {

    val result = usersAnswers map ((_, 1)) groupBy (_._1) map (v => (v._1, v._2 reduce(_._2 + _._2)))

    result zip possibleAnswers map (v => (v._2._2, v._1._2))
  }

  final override def vote(answer: Int): Unit = {

      if (possibleAnswers.contains(answer))
        usersAnswers += answer
      else throw IllegalArgumentException

  }
}

case class OpenQuiz[String](question: String) extends Quiz[String]{

  final private val quiz = question

  var usersAnswers: List[String] = List()

  final override def result = usersAnswers

  final override def vote(answer: Any) = {
    usersAnswers += answer
  }
}

object QuizType extends Enumeration {
  val MULTI, OPEN, CHOICE = Value
}
