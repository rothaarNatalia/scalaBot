package bot2.polls

import bot2.Visibility
import org.joda.time.DateTime

case class Poll(userId: String,
                isAnonymous: Option[Boolean],
                visibility: Option[Visibility.Value],
                dateFrom: Option[DateTime],
                dateTo: Option[DateTime]) {


  private val questions: List[String] = List()
  private val answers: List[String] = List()

  def isCorrect: Boolean = {

    Seq(dateTo.isDefined, dateFrom.isDefined, visibility.isDefined, isAnonymous.isDefined).foldLeft(false)(_ && _)

    ???
  }

  def addQuestion = ???

  def deleteQuestion = ???

}
