package bot

import java.util.Calendar

import scala.collection.immutable.HashMap

/**
  * Created by rothaar on 04.02.2018.
  */

case class Vote(uuid: String,
                owner: String,
                isAnonymous: Boolean,
                visibility: String,
                dateFrom: Calendar,
                dateTo: Calendar
               ) {

  private

  var result: Int  = 0

  def voteResult: Int  = {

    ???
  }

  def deleteQuestion(id: Int) = {}

  def addQuestion(id: Int) = {}

  def vote(id: Int) = {

  }
}

object Votes {

  private var votes: HashMap[String, Vote] = HashMap[String, Vote]()
  private var votesStatuses: HashMap[String, Boolean] = HashMap[String, Boolean]()

  def addVote(uuid: String,
              owner: String,
              isAnonymous: Boolean,
              visibility: String,
              dateFrom: Calendar,
              dateTo: Calendar) ={

    votes += (uuid -> Vote(uuid, owner, isAnonymous, visibility, dateFrom, dateTo))
    votesStatuses += (uuid -> false)

  }

  def startVote(uuid: String) = {
    votesStatuses += (uuid -> true)
  }

  def stopVote(uuid: String) = {
    votesStatuses += (uuid -> false)
  }

  def deleteVote(uuid: String) = {
    votesStatuses -= uuid
    votes -= uuid
  }

  def showActiveVotes() = {

    votesStatuses withFilter (_._2) map (_._1)
  }

  def showResult(uuid: String): Option[Int] = {

    if (votes.contains(uuid)) {
      val vote = votes(uuid)
      if ((vote.visibility == "CONTINIOUS") || () )
        Option(vote.voteResult)
    }
    else None
  }

}


