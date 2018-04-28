package bot2


import bot2.PollManager.polls
import bot2.polls.{Poll, Quiz, UserId}
import org.joda.time.DateTime

import scala.util.Random

object PollManager {

  private var polls: Map[Long, Poll] = Map()
  private var sessions: Map[UserId, Long] = Map()

  val idGenerator = new Random()

  private def addPoll(userId: UserId, poll: Poll): Option[(Long, (Poll, List[UserId]))] = {
    if(poll.isCorrect) {
    val id = idGenerator.nextLong()
        Option((id, (poll, Nil)))
    }
    else None
  }

  private def deletePoll(userId: UserId, id: Long): Option[Long] = {

    if (!polls.contains(id))
      None

    if ((polls(id).userId == userId))
      Some(id)
    else None

  }

  private def startPoll(userId: UserId, id: Long): Option[Long] = {

    if (!polls.contains(id))
      None

    val poll = polls(id)

    if ((poll.dateFrom.isEmpty) && (poll.userId == userId) && (!poll.isActive))
        Some(id)
    else None

  }

  private def stopPoll(userId: UserId, id: Long): Option[Long] = {

    if (!polls.contains(id))
      None

    val poll = polls(id)

    if ((poll.dateTo.isEmpty) && (poll.userId == userId) && (poll.isActive))
      Some(id)
    else None

  }

  private def result(id: Long): Option[List[String]] = {

    if (!polls.contains(id))
      None
    else polls(id).result

  }

  private def list(userId: UserId): List[String] = {
    polls map (p => s"#${p._1} ${p._2.name}") toList
  }

  private def begin(userId: UserId, pId: Long) = {

    polls(pId).copy()
    ???
  }

  private def addQuestion(userId: UserId, q: Quiz): Option[Long] = {
    ???
  }

  def execute(user: UserId, cmd: Command): String = {

    cmd match {
      case a: CreatePoll => {val p = Poll(userId = user,
                                  name = a.name,
                                  visibility = a.visibility,
                                  isAnonymous = a.anonymous,
                                  dateFrom = a.dateFrom,
                                  dateTo = a.dateTo)

        val elem = addPoll(userId = user, poll = p)
        if (elem.isDefined)
            polls = polls + elem.get

      }

      case d: DeletePoll => {
        val id = deletePoll(userId = user, id = d.id)
        polls = polls - id.getOrElse(Long.MinValue)
      }
      case srt: StartPoll => startPoll(userId = user, id = srt.id)
      case stp: StopPoll => stopPoll(userId = user, id = stp.id)
      case rsl: Result => result(id = rsl.id)
      case _: PollsList.type => list(userId = user)
    }

    ???
  }

}
