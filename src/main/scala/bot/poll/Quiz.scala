package bot.poll


case class Quiz(quiz: String,
                quizType: QuizType.Value,
                answers: Vector[(Option[UserId], Answer[_])],
                pAnswers: List[String]) {

  private val possibleAnswers = (1L to pAnswers.length) zip (pAnswers) toMap

  import QuizType._


  def answer(u: Option[UserId], a: Answer[_]): Option[(Option[UserId],Answer[_])] = {

    def answerIsCorrect(a: Answer[_]): Boolean = {

      quizType match {
        case MULTI => {a match {
          case Answer(x: List[_]) => {
            if (!(x.distinct.length == x.length))
              false
            else
              x.view.collect({case a: Long => possibleAnswers.contains(a)
              case _ => false}) reduceOption (_ && _) getOrElse (false)}
        }}

        case CHOICE | MULTI => { a match {
          case Answer(xs: Long) => possibleAnswers.contains(xs)
        }

        }
        case OPEN => true
      }

    }

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
       |${possibleAnswers.view.map(v => "# "+ v._1 + ": " + v._2 + "\n").reduceOption(_ + " " + _).getOrElse("")}
     """.stripMargin
  }

  def result(isAnonymous: Boolean): String = {

    def histogram: String = {

      val starCount = 13 //magic

      val usrAswrs = answers.view.collect{
        case (usr, Answer(x: Long)) => List((usr, x))
        case (usr,Answer(xs: List[_])) => xs.collect({case a: Long => (usr, a)})
      }.flatten.groupBy(_._2)

      val answered = usrAswrs.map(v => (v._1, v._2.size))

      val ids = answered.keySet
      val nonAnswered = possibleAnswers withFilter (id => !ids.contains(id._1)) map (v => (v._1, 0))

      val totalVotes = answered.view map (_._2) sum
      val k = if (totalVotes == 0) 1 else starCount/totalVotes

      val histogram = (answered ++ nonAnswered).view.
        map(v =>  {s"${"# "+ v._1 + ": " + possibleAnswers(v._1)}   ${ "*" * (k * v._2)} \n"}).
        reduceOption(_ + _) getOrElse("")

      val users = if (isAnonymous)
        ""
      else
        (usrAswrs).view.
          map(v => s"${"# "+ v._1}: ${v._2.view.map(_._1.getOrElse("") + " ") reduceOption(_ + _) getOrElse("")} \n").
          reduceOption(_ + _) getOrElse ("")

      histogram + "\n" + users

    }

    def list: String = {

      if (isAnonymous)
        answers.view.map(_._2 + " ") reduceOption(_ + _) getOrElse ("")
      else
        answers.view.map(v => s"${(v._1).getOrElse("")}: ${v._2} \n") reduceOption(_ + _) getOrElse ("")

    }

    quizType match {
      case MULTI | CHOICE => histogram
      case OPEN => list
      case _ => ""
    }

  }

}

object QuizType extends Enumeration {

  val MULTI, CHOICE, OPEN = Value
}
