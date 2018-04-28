package bot2.polls

import bot2.Answer

case class Quiz(quizType: QuizType.Value, quiz: String, pAnswers: String*) {

  private var answers: Map[UserId, Seq[Answer[_]]] = Map.empty
  private val possibleAnswers = (0 to pAnswers.length - 1) zip (pAnswers) toMap


  import QuizType._

  private def isCorrect(a: Answer[_]*): Boolean = {

    quizType match {
      case MULTI => a forall (v => {val asw: Int = Integer.parseInt(v.answer.toString); possibleAnswers.contains(asw)})
      case CHOICE => a.headOption map (v => {val asw: Int = Integer.parseInt(v.answer.toString); possibleAnswers.contains(asw)}) getOrElse(false)
      case OPEN => true
    }

  }

  def answer(u: UserId, a: Answer[_]*): Option[(UserId, Seq[Answer[_]])] = {

    if (isCorrect(a: _*))
      Option((u, a))
    else None

  }

  def view = ???

}
