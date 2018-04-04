package bot2

import bot2.polls.Poll

import scala.util.Random

object PollManager {

  private var polls: Map[Long, Poll] = Map()
  val idGenerator = new Random()

  private def addPoll(userId: String, poll: Poll): Long = {
    val id = idGenerator.nextLong()
    polls += (id -> poll)
    id
  }

  private def deletePoll(userId: String, id: Long): Unit = {
    polls -= id
  }

  private def startPoll(userId: String, id: Long): Unit = {
    polls -= id
  }

  private def stopPoll(userId: String, id: Long): Unit = {
    polls -= id
  }

  private def result(userId: String, id: Long): Unit = {
    polls -= id
  }

  private def list(userId: String): Unit = {
    //polls -= id
  }

  def execute(user: String, cmd: Command): String = {

    cmd match {
      case a: CreatePoll => addPoll(userId = user, Poll(userId = user,
                                                         visibility = a.visibility,
                                                         isAnonymous = a.anonymous,
                                                         dateFrom = a.dateFrom,
                                                         dateTo = a.dateTo))
      case d: DeletePoll => deletePoll(userId = user, id = d.id)
      case srt: StartPoll => startPoll(userId = user, id = srt.id)
      case stp: StopPoll => stopPoll(userId = user, id = stp.id)
      case rsl: Result => result(userId = user, id = rsl.id)
      case _: PollsList.type => list(userId = user)
    }

    ???
  }

}
