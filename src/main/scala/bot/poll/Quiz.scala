package bot.poll


case class Quiz(quiz: String,
                quizType: QuizType.Value,
                answers: Vector[(Option[UserId], Answer[_])],
                pAnswers: List[String]) {

  val possibleAnswers = (1L to pAnswers.length) zip (pAnswers) toMap

  import QuizType._

  private def answerIsCorrect(a: Answer[_]): Boolean = {

    quizType match {
      case MULTI => {a match {
                        case Answer(x: List[_]) => {
                          if (!(x.distinct.length == x.length))
                            false
                          else
                          x.view.collect({case a: Long => possibleAnswers.contains(a)
                                          case _ => false}) reduce (_ && _)}
                        case _ => false
                    }}

      case CHOICE => { a match {
                          case Answer(xs: Long) => possibleAnswers.contains(xs)
                          case _ => false
                        }

      }
      case OPEN => true
    }

  }

  def answer(u: Option[UserId], a: Answer[_]): Option[(Option[UserId],Answer[_])] = {

    if (answerIsCorrect(a))
        Some(u, a)
    else
        None

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
