package bot.pools

import java.util.{Calendar, TimeZone, UUID}

import bot.{Response, Responses}

import scala.collection.immutable.HashMap

/**
  * Created by rothaar on 04.02.2018.
  */

object Polls {

  private var pools: HashMap[String, Poll] = HashMap[String, Poll]()

  //TODO active pending stopped deleted
  private var poolsStatuses: HashMap[String, PollStatus.Value] = HashMap[String, PollStatus.Value]()

  private def pollExists(uuid: String): Boolean = pools contains(uuid)

  def addPoll(owner: String,
              isAnonymous: Boolean,
              visibility: Visibility.Value,
              dateFrom: Option[Calendar],
              dateTo: Option[Calendar]): Response ={

    val uuid: String = UUID.randomUUID().toString
    pools += (uuid -> Poll(owner, isAnonymous, visibility, dateFrom, dateTo))
    poolsStatuses += (uuid -> PollStatus.PENDING)

    uuid
  }

  def startPoll(uuid: String, user: String): Response = {

    if (!pollExists(uuid))
      return Responses.NotExist

    //if (poolsStatuses)

    pools(uuid).canStart(user, Calendar.getInstance(TimeZone.getDefault)) match {
      case Responses.OK => poolsStatuses += (uuid -> PollStatus.ACTIVE); Responses.OK
      case _ => Responses.Denied
    }

  }

  def stopPoll(uuid: String, user: String): Response = {

    if (!pollExists(uuid))
      return Responses.NotExist

    pools(uuid).canStop(user, Calendar.getInstance(TimeZone.getDefault)) match {
      case Responses.OK => poolsStatuses += (uuid -> PollStatus.STOPPED); Responses.OK
      case _ => Responses.Denied
    }
  }

  def deletePoll(uuid: String, user: String) = {
    poolsStatuses -= uuid
    pools -= uuid
  }

  def showActivePolls() = {

    poolsStatuses withFilter (_._2 == PollStatus.ACTIVE) map (_._1)
  }

  def showResult(uuid: String): Option[Int] = {

    if (pollExists(uuid)) {
      val pool = pools(uuid)

        Option(pool.result)
    }
    else None
  }

}

object PollStatus extends Enumeration {
  val ACTIVE, PENDING, STOPPED = Value
}


