package bot2.polls

import bot2.{Visibility}
import com.github.nscala_time.time.Imports._


import scala.util.Random

case class Poll(userId: UserId,
                name: String,
                isAnonymous: Option[Boolean],
                visibility: Option[Visibility.Value],
                dateFrom: Option[DateTime],
                dateTo: Option[DateTime],
                questions: Map[Long, Quiz],
                isActive: Boolean = false) {

  var answered: Map[Long, Vector[UserId]] = questions map (_._1 -> Vector.empty)

  def result: Option[List[String]] = {

    val vsb = visibility.getOrElse(Visibility.AFTERSTOP)

    if ((vsb == Visibility.CONTINUOUS) || ((vsb == Visibility.AFTERSTOP) && (!isActive)))
      None//Some(questions)
    else
      None

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


  def answer(userId: UserId, qId: Long, a: Answer[_]*) = {

    val currentDate = DateTime.now()

    if ((!questions.contains(qId)) ||
        (currentDate < dateFrom.getOrElse(currentDate)) ||
        (currentDate > dateTo.getOrElse(currentDate)))
      None

    if (answered(qId).contains(userId))
      None

    val result =

    if(isAnonymous.getOrElse(true))
      questions(qId).answer(None, a: _ *)
    else
      questions(qId).answer(Some(userId), a: _ *)

    if (result.nonEmpty) {
      val usrs = answered(qId)
      answered.updated(qId, usrs ++ userId)

    }

  }

  def view = {

    ???
  }

  def addQuestion(q: Quiz): (Long, Quiz) = (Random.nextLong(), q)

  def deleteQuestion(qId: Long): Option[(Long, Quiz)] = {
    if (questions.contains(qId))
        Some(qId, questions(qId))
    else
        None
  }

}
