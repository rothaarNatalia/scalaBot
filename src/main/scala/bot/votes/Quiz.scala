package bot.votes

import scala.collection.immutable.HashMap

case class Quiz(`type`: QuizType.Value, question: String){

  private val quizType = `type`

  private val quiz = question

  def possibleAnswers: HashMap[Int, String] = HashMap()



}

object QuizType extends Enumeration {
  val MULTI, OPEN, CHOICE = Value
}
