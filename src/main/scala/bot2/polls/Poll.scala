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

  def result: String = {

    val vsb = visibility.getOrElse(Visibility.AFTERSTOP)

    if ((vsb == Visibility.CONTINUOUS) || ((vsb == Visibility.AFTERSTOP) && (!isActive)))
      ""//Some(questions)
    else
      ""

  }

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
      quiz.answer(None, a: _ *)
    else
      quiz.answer(Some(userId), a: _ *)

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
       |${questions.mapValues(v => v.quiz + "\n").view.reduce(_ + _)}
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
