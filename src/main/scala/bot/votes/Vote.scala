package bot.votes

import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.immutable.HashMap

case class Vote( //uuid: String,
                     owner: String,
                     isAnonymous: Boolean,
                     visibility: Visibility.Value,
                     dateFrom: Calendar,
                     dateTo: Calendar ){

      private var quizList: HashMap[Int, Quiz] = HashMap()

      private var quizIdCounter: AtomicInteger = new AtomicInteger(1)

      def result: Int = 0

      def addQuiz(q: Quiz): Int = {

            val id = quizIdCounter.getAndAdd(1)
            quizList += ( id -> q)

            id
      }

      def deleteQuiz(id: Int) = {

         quizList -= id
      }

      def vote(quizId: Int, answer: String) = {

          if(quizList.contains(quizId))
                quizList(quizId).vote(answer)
      }
}

object Visibility extends Enumeration {

      val AFTERSTOP, CONTINIOUS = Value
}