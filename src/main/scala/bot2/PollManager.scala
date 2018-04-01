package bot2

import bot2.polls.Poll

import scala.util.Random

object PollManager {

  private var polls: Map[Long, Poll] = Map()
  val idGenerator = new Random()

  def addPoll(poll: Poll): Long = {
    val id = idGenerator.nextLong()
    polls += (id -> poll)
    id
  }

  def deletePoll(id: Long): Unit = {
    polls -= id
  }

}
