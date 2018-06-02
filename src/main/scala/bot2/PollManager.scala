package bot2


import bot2.polls.{Answer, Poll, Quiz, UserId}
import org.joda.time.DateTime

import scala.util.Random

object PollManager {

  private var polls: Map[Long, Poll] = Map()
  private var sessions: Map[UserId, Long] = Map()

  val idGenerator = new Random()

  private def addPoll(userId: UserId, poll: Poll): Option[(Long, Poll)] = {
    if(poll.isCorrect) {
    val id = idGenerator.nextLong()
        Option((id, poll))
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

  private def startPoll(userId: UserId, id: Long): Option[Poll] = {

    if (!polls.contains(id))
      None

    val poll = polls(id)

    if ((poll.dateFrom.isEmpty) && (poll.userId == userId) && (!poll.isActive))
        Some(poll.copy(dateFrom = Some(DateTime.now()),  isActive = true))
    else None

  }

  private def stopPoll(userId: UserId, id: Long): Option[Poll] = {

    if (!polls.contains(id))
      None

    val poll = polls(id)

    if ((poll.dateTo.isEmpty) && (poll.userId == userId) && (poll.isActive))
      Some(poll.copy(isActive = false, dateTo = Option(DateTime.now())))
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

  private def begin(userId: UserId, pId: Long): Option[(UserId, Long)] = {

    if (sessions.contains(userId))
      None
    else
      Some((userId, pId))
  }

  private def end(userId: UserId): Option[(UserId, Long)] = {

    if (sessions.contains(userId))
      None
    else
      Some((userId, sessions(userId)))
  }

  private def addQuestion(userId: UserId, q: Quiz): Option[(Long, Quiz)] = {

    if (!sessions.contains(userId))
      None

    val p = polls(sessions(userId))

    if (p.userId != userId)
        None
      else{
        Some(p.addQuestion(q))
    }

  }

  private def deleteQuestion(userId: UserId, qId: Long): Option[Poll] = {

    if (!sessions.contains(userId))
      None

    val p = polls(sessions(userId))

    if (p.userId != userId)
      None
    else{
        p.deleteQuestion(qId) flatMap (v => Some(p.copy(questions = (p.questions - v._1))))
    }

  }

  private def answer(userId: UserId, qId: Long, a: Answer[_]*): Option[Quiz] = {

    if (!sessions.contains(userId))
      None

    val pollId = sessions(userId)

    if (!polls.contains(pollId))
      None

    val poll = polls(pollId)

    if(!poll.questions.contains(qId))
      None

    if (poll.answered(qId).contains(userId))
      None

    poll.answer(userId, qId, a: _*)

  }

  def execute(user: UserId, cmd: Command): String = {

      (cmd match {
        case crtPoll: CreatePoll => {
                                        val p = Poll(userId = user,
                                          name = crtPoll.name,
                                          visibility = crtPoll.visibility,
                                          isAnonymous = crtPoll.anonymous,
                                          dateFrom = crtPoll.dateFrom,
                                          dateTo = crtPoll.dateTo,
                                          questions = Map.empty)

                                        val elem = addPoll(userId = user, poll = p)
                                        if (elem.isDefined) {
                                          val poll = elem.get
                                          polls = polls + poll
                                          s"Umfrage mit Id ${poll._1} war erstellt"
                                        }
        }


        case dltPoll: DeletePoll => {
                                        val pollId = deletePoll(userId = user, id = dltPoll.id)

                                        if (pollId.isDefined) {
                                          val id = pollId.get
                                            polls = polls - id
                                          s"Umfrage mit Id ${id} war entfernt"
                                        }
                                    }

        case srtPoll: StartPoll => {
                                        val poll = startPoll(userId = user, id = srtPoll.id)
                                        if (poll.isDefined) {
                                          polls = polls.updated(srtPoll.id, poll.get)
                                          s"Umfrage mit Id ${srtPoll.id} war gestartet"
                                        }

                                   }
        case stpPoll: StopPoll => {
                                    val poll = stopPoll(userId = user, id = stpPoll.id)
                                    if (poll.isDefined) {
                                      polls = polls.updated(stpPoll.id, poll.get)
                                      s"Umfrage mit Id ${stpPoll.id} war gestoppt"
                                    }
                                  }
        case rsl: Result => result(id = rsl.id)
        /*case _: PollsList.type => list(userId = user)
        */

      }) match {
        case msg: String => msg
        case _ => "Schief gegangen"
      }

    }

}
