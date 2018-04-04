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

    dateTo.fold()

  }

  def addQuestion

  def deleteQuestion

}
