package bot.pools

import bot.{Answer, Result, UserId}

import scala.collection.immutable.HashMap

abstract class Quiz{

  protected var usersAnswers: List[(Answer, UserId)] = List[(Answer, UserId)]()

  val quiz: String

  def createQuiz(quizType: QuizType.Value, question: String, answers: Option[HashMap[Int, Answer]]): Quiz = {

    quizType match {
      case QuizType.CHOICE => if (answers == None) throw IllegalArgumentException
                                    else ChoiceQuiz(question, answers.get)
      case QuizType.MULTI => if (answers == None) throw IllegalArgumentException
                                      else MultiQuiz(question, answers.get)
      case QuizType.OPEN =>  OpenQuiz(question)
    }
  }

  def answer(userAnswer: Answer, userId: UserId)

  def result: List[Result]


}

case class MultiQuiz(question: String, answers: HashMap[Int, Answer]) extends Quiz{

  final val quiz = question

  final private val possibleAnswers: HashMap[Int, Answer] = answers

  private def answerIsPossible(userAnswer: Answer): Boolean =
    possibleAnswers.contains(userAnswer.asInstanceOf[Int])

  final override def result: List[Result] = {

    val result = (usersAnswers map (a => (a._1, 1, a._2)) groupBy (_._1)) map
                                                      (v => (v._1, v._2 map (_._2) reduce(_ + _), v._2 map (_._3)))

    result zip possibleAnswers map (v => (v._2._2, v._1._2, Option(v._1._3))) toList

  }

  final override def answer(userAnswer: Answer, userId: UserId) = {

    val answersSeq = userAnswer.asInstanceOf[Seq[Answer]]

    val hasDuplicates = (answersSeq groupBy(_)  exists(_._2.length > 1)).asInstanceOf[Boolean]

    if (hasDuplicates)
      throw IllegalArgumentException

    answersSeq foreach (a =>
              if (answerIsPossible(a))
                usersAnswers = (a, userId) :: usersAnswers
              else throw IllegalArgumentException
      )
  }
}

case class ChoiceQuiz(question: String, answers: HashMap[Int, Answer]) extends Quiz{

  final val quiz = question

  final private val possibleAnswers: HashMap[Int, Answer] = answers

  private def answerIsPossible(answer: Answer): Boolean =
    possibleAnswers.contains(answer.asInstanceOf[Int])

  final override def result: List[Result] = {

    val result = (usersAnswers map (a => (a._1, 1, a._2)) groupBy (_._1)) map
      (v => (v._1, v._2 map (_._2) reduce(_ + _), v._2 map (_._3)))

    result zip possibleAnswers map (v => (v._2._2, v._1._2, Option(v._1._3))) toList

  }

  final override def answer(userAnswer: Answer, userId: UserId): Unit = {

      if (answerIsPossible(userAnswer))
        usersAnswers = (userAnswer, userId) :: usersAnswers
      else throw IllegalArgumentException

  }
}

case class OpenQuiz(question: String) extends Quiz {

  final val quiz = question

  final override def result: List[Result] = {

    (usersAnswers map (a => (a._1, 1, a._2)) groupBy (_._1)) map
      (v => (v._1, v._2 map (_._2) reduce(_ + _), Option(v._2 map (_._3)))) toList

  }

  final override def answer(userAnswer: Answer, userId: UserId) = {

    usersAnswers = (userAnswer, userId) :: usersAnswers
  }

}

object QuizType extends Enumeration {
  val MULTI, OPEN, CHOICE = Value
}
