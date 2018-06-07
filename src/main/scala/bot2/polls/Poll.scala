package bot2.polls

import com.github.nscala_time.time.Imports._

import scala.util.Random

case class Poll(userId: UserId,
                name: String,
                isAnonymous: Option[Boolean],
                visibility: Option[Visibility.Value],
                dateFrom: Option[DateTime],
                dateTo: Option[DateTime],
                questions: Map[Long, Quiz],
                var answered: Map[Long, Set[UserId]],
                isActive: Boolean = false
                ) {
//= questions map (_._1 -> Vector.empty)

  def isCorrect: Boolean = {

    val s = Seq(isAnonymous.isDefined, visibility.isDefined, dateFrom.isDefined, dateTo.isDefined)

    val seqCheck = !(s zip s.tail).contains((false, true))

    val dFCheck = if (dateFrom.isDefined)
      dateFrom.exists(_.isAfterNow)
    else true

    val dTCheck = if (dateTo.isDefined)
      dateTo.exists(_.isAfter(dateFrom.get))
    else true


    seqCheck && dFCheck && dTCheck

  }

  def result: String = {

    import QuizType._

    def histogram(q: Quiz): String = {

      val starCount = 13 //magic

      val usrAswrs = q.answers.view.flatMap(v => v._2 map(a => (v._1, a))).groupBy(_._2)

      val aswrs = usrAswrs.map(mr => (mr._1, mr._2.length))

      val histo = aswrs.view.map(v =>  {s"$v._1   ${ "*" * (starCount / v._2)} \n"}).reduce(_ + _)


      val usrs = if (isAnonymous == Some(true)) ""
                 else usrAswrs.view.map(v => s"${v._1}: ${v._2.view.map(_._1.getOrElse("") + " ")} \n") reduce(_ + _)

      histo + "\n" + usrs

    }

    def list(q: Quiz): String = {

      val usrAswrs = q.answers.view.flatMap(v => v._2 map(a => (v._1, a)))

      if (isAnonymous == Some(true))
        usrAswrs.view.map(_._2 + " ") reduce(_ + _)
      else
        usrAswrs.view.map(v => s"${v._1.getOrElse("")}: $v._2 \n") reduce(_ + _)

    }

    val vsb = visibility.getOrElse(Visibility.AFTERSTOP)

    if ((vsb == Some(Visibility.CONTINUOUS)) || ((vsb == Some(Visibility.AFTERSTOP)) && (!isActive))){
      questions.view map (q => {
        s"""Die Frage mit Id ${q._1}
           |Das Ergebniss:
           |${
          q._2.quizType match {
            case MULTI | CHOICE => histogram(q._2)
            case OPEN => list(q._2)
          }
        }
           |
         """.stripMargin
      }) reduce(_ + _)
    }
    else
      "Noch keine Ergebnisse"

  }

  def answer(userId: UserId, qId: Long, a: Answer[_]*): Option[Quiz] = {

    val currentDate = DateTime.now()

    if ((!questions.contains(qId)) ||
        (currentDate < dateFrom.getOrElse(currentDate)) ||
        (currentDate > dateTo.getOrElse(currentDate)))
      None

    if (answered(qId).contains(userId))
      None

    val quiz: Quiz = questions(qId)

    val result =

    if(isAnonymous.getOrElse(true))
      quiz.answer(None, a.distinct: _ *)
    else
      quiz.answer(Some(userId), a.distinct: _ *)

    if (result.nonEmpty) {
      val usrs = answered(qId)
      answered.updated(qId, usrs ++ userId)
      Some(quiz.copy(answers = ( result.get +: quiz.answers)))
    }
    else
      None

    ???
  }

  def view: String = {

    s"""
       |Die Umfrage $name
       |Die Fragen:
       |${questions.view.map(v => v._2.quiz + "\n").reduce(_ + _)}
       |
     """.stripMargin
  }

  def addQuestion(q: Quiz): (Long, Quiz) = {

    val qId = Random.nextLong()

    answered = answered.updated(qId, Set.empty)

    (qId, q)
  }

  def deleteQuestion(qId: Long): Option[(Long, Quiz)] = {
    if (questions.contains(qId)) {
      answered = answered - qId
      Some(qId, questions(qId))
    }
    else
        None
  }

}


object Visibility extends Enumeration {

  val AFTERSTOP, CONTINUOUS = Value

}
