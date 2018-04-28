package bot2.polls

import bot2.Visibility
import org.joda.time.DateTime

case class Poll(userId: UserId,
                name: String,
                isAnonymous: Option[Boolean],
                visibility: Option[Visibility.Value],
                dateFrom: Option[DateTime],
                dateTo: Option[DateTime],
                isActive: Boolean = false) {


  private var questions: Map[Long, Quiz] = Map.empty

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

  def addQuestion(q: Quiz) = {

    ???
  }

  def deleteQuestion = ???

  def answer(id: Long) = {

    }

  def view = {

  }

  def begin(userId: UserId, id: Long) = ???

  def end(userId: UserId) = ???



}
