package bot.pools

import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

import bot._

import scala.collection.immutable.HashMap

case class Poll( //uuid: String,
                 owner: String,
                 isAnonymous: Boolean,
                 visibility: Visibility.Value,
                 dateFrom: Option[Calendar] = None,
                 dateTo: Option[Calendar] = None ){

      private var quizList: HashMap[Int, Quiz] = HashMap()

      private var quizIdCounter: AtomicInteger = new AtomicInteger(1)

      def result: List[(String, List[Result])] = {

        val mappedResults = quizList mapValues (q => ({
                                val result =  q.result map ( v =>  if (isAnonymous) (v._1, v._2, None) else v)
                                (q.quiz, result)
                              }))

        (mappedResults values) toList
      }

      def addQuiz(q: Quiz): Int = {

            val id = quizIdCounter.getAndAdd(1)
            quizList += ( id -> q)

            id
      }

      def deleteQuiz(id: Int) = {

         quizList -= id
      }

      def answer(quizId: Int, answer: Answer, userId: UserId) = {

          if(quizList.contains(quizId))
                quizList(quizId).answer(answer, userId)
      }

      def canStart(user: String, date: Calendar): Response ={
            if ((owner == user) && (dateFrom == None))
                  Responses.OK
            else
                  Responses.Denied
      }

      def canStop(user: String, date: Calendar): Response ={
            if ((owner == user) && (dateTo == None))
                  Responses.OK
            else
                  Responses.Denied
      }
}

object Visibility extends Enumeration {

      val AFTERSTOP, CONTINIOUS = Value
}