package bot2.polls

import bot2.UserAnswer

case class Quiz(quiz: String,
                quizType: QuizType.Value,
                answers: Vector[(Option[UserId], Seq[Answer[_]])],
                pAnswers: List[String]) {

  //private val answers: Vector[(Option[UserId], Seq[Answer[_]])] = Vector.empty
  private val possibleAnswers = (1L to pAnswers.length) zip (pAnswers) toMap

  import QuizType._

  private def answerIsCorrect(a: Answer[_]*): Boolean = {

    quizType match {
      case MULTI => {a forall (_ match {
        case Answer(x: Long) => possibleAnswers.contains(x)
        case _ => false
      } )}

      case CHOICE => { a match {
        case Answer(xs: Long) :: Nil => possibleAnswers.contains(xs)
        case _ => false
      }

      }
      case OPEN => true
    }

  }

  def answer(u: Option[UserId], a: Answer[_]*): Option[(Option[UserId], Seq[Answer[_]])] = {

    if ((!answerIsCorrect(a: _*)) )//|| (answered.contains(u)))
      None
    else
      Some(u, a)

  }

  def view = {
    s"""
       |Die Frage $quiz
       |Der Typ ${quizType}
       |Die moegliche Antworten sind
       |${pAnswers.reduce(_ + " " + _)}
     """.stripMargin
  }

}

object QuizType extends Enumeration {

  val MULTI, CHOICE, OPEN = Value
}
