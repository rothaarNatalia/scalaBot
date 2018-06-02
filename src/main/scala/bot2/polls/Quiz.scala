package bot2.polls

import bot2.UserAnswer

case class Quiz(quizType: Option[QuizType.Value],
                quiz: String,
                answers: Vector[(Option[UserId], Seq[Answer[_]])],
                pAnswers: List[String]) {

  //private val answers: Vector[(Option[UserId], Seq[Answer[_]])] = Vector.empty
  private val possibleAnswers = (0 to pAnswers.length - 1) zip (pAnswers) toMap

  import QuizType._

  private def isCorrect(a: Answer[_]*): Boolean = {

    quizType match {
      case MULTI => a forall (v => {val asw: Int = Integer.parseInt(v.answer.toString); possibleAnswers.contains(asw)})
      case CHOICE => a.headOption map (v => {val asw: Int = Integer.parseInt(v.answer.toString); possibleAnswers.contains(asw)}) getOrElse(false)
      case OPEN => true
    }

  }

  def answer(u: Option[UserId], a: Answer[_]*): Option[(Option[UserId], Seq[Answer[_]])] = {

    if ((!isCorrect(a: _*)) )//|| (answered.contains(u)))
      None
    else
      Some(u, a)

  }

  def view = {
    ""
  }

}

object QuizType extends Enumeration {

  val MULTI, CHOICE, OPEN = Value
}
