package bot2


import bot2.polls._
import org.joda.time.DateTime

import scala.util.Random

object PollManager {

  private var polls: Map[Long, Poll] = Map()
  private var sessions: Map[UserId, Long] = Map()

  val idGenerator = new Random()

  private def getPollFromSession(userId: UserId): Option[(Long, Poll)] = {

    if (!sessions.contains(userId))
      None

    val pollId = sessions(userId)

    if (!polls.contains(pollId))
      None

    Some(pollId, polls(pollId))
  }

  private def getPoll(pId: Long): Option[Poll] = {

    if (!polls.contains(pId))
      None
    else Some(polls(pId))
  }

  private def addPoll(userId: UserId, poll: Poll): Option[(Long, Poll)] = {
    if(poll.isCorrect) {
    val id = idGenerator.nextLong()
        Option((id, poll))
    }
    else None
  }

  private def deletePoll(userId: UserId, id: Long): Option[Long] = {

    getPoll(id) flatMap( p =>  if (p.userId == userId) Some(id) else None)

  }

  private def startPoll(userId: UserId, id: Long): Option[(Long, Poll)] = {

    getPoll(id) flatMap( p =>

                      if ((p.dateFrom.isEmpty) && (p.userId == userId) && (!p.isActive))
                        Some(id, p.copy(dateFrom = Some(DateTime.now()),  isActive = true))
                    else None )

  }

  private def stopPoll(userId: UserId, id: Long): Option[(Long, Poll)] = {

    getPoll(id) flatMap( p =>

    if ((p.dateTo.isEmpty) && (p.userId == userId) && (p.isActive))
      Some(id, p.copy(isActive = false, dateTo = Option(DateTime.now())))
    else None)

  }

  private def result(id: Long): Option[String] = {

    getPoll(id) flatMap( p => Some(p.result))
  }

  private def list(userId: UserId): Option[String] = {

    Some("Liste von den Umfragen: \n" + polls.map(p => s"#${p._1} ${p._2.name}\n").reduce(_ + _))

  }

  private def view(userId: UserId): Option[String] = {

    getPollFromSession(userId) flatMap(p => Some(p._2.view))

  }

  private def begin(userId: UserId, pId: Long): Option[(UserId, Long)] = {

    if ((sessions.contains(userId)) || (!polls.contains(pId)))
      None
    else
      Some((userId, pId))
  }

  private def end(userId: UserId): Option[(UserId, Long)] = {

    if (!sessions.contains(userId))
      None
    else
      Some((userId, sessions(userId)))
  }

  private def addQuestion(userId: UserId, q: Quiz): Option[(Long, Poll)] = {

    getPollFromSession(userId) flatMap (p => if (p._2.userId != userId) None
                                                else {
                                            val quiz = p._2.addQuestion(q)
                                            Some(p._1, p._2.copy(questions = (p._2.questions + quiz)))})

  }

  private def deleteQuestion(userId: UserId, qId: Long): Option[(Long, Poll)] = {

    getPollFromSession(userId) flatMap (p =>

      if (p._2.userId != userId)
      None
    else{
        p._2.deleteQuestion(qId) flatMap (v => Some(p._1, p._2.copy(questions = (p._2.questions - v._1))))
    })

  }

  private def answer(userId: UserId, qId: Long, a: Answer[_]*): Option[(Long, Poll)] = {

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

    poll.answer(userId, qId, a: _*) flatMap (q => Some(pollId,
                                                       poll.copy(questions = poll.questions.updated(qId, q),
                                                                 answered =  {if (poll.answered.contains(qId)) {
                                                                   val aswUsers = poll.answered(qId);
                                                                   poll.answered.updated(qId, (aswUsers + userId))
                                                                 }
                                                                 else
                                                                   poll.answered + (qId -> Set(userId))})
                                                       ))
  }

  def execute(user: UserId, cmd: Command): String = {

      (cmd match {
        case asw: UserAnswer => {
                                      answer(user, asw.id, asw.a: _*).
                                        flatMap(p => {polls = polls.updated(p._1, p._2); Some(s"Ihrer Antwort auf die Frage ${asw.id} war gespeichert")})
                                }
        case crtPoll: CreatePoll => {
                                        val p = Poll(userId = user,
                                          name = crtPoll.name,
                                          visibility = crtPoll.visibility,
                                          isAnonymous = crtPoll.anonymous,
                                          dateFrom = crtPoll.dateFrom,
                                          dateTo = crtPoll.dateTo,
                                          questions = Map.empty,
                                          answered = Map.empty)

                                        addPoll(userId = user, poll = p)
                                          .flatMap(poll => {polls = polls + poll; Some(s"Die Umfrage mit Id ${poll._1} war erstellt")})
                                    }
        case dltPoll: DeletePoll => {
                                        deletePoll(userId = user, id = dltPoll.id)
                                          .flatMap(id => {polls = polls - id; Some(s"Die Umfrage mit Id ${id} war entfernt")})
                                    }
        case srtPoll: StartPoll => {
                                        startPoll(userId = user, id = srtPoll.id)
                                          .flatMap(p => {polls = polls.updated(p._1, p._2); Some(s"Die Umfrage mit Id ${p._1} war gestartet")})

                                   }
        case stpPoll: StopPoll => {
                                    stopPoll(userId = user, id = stpPoll.id)
                                      .flatMap(p => {polls = polls.updated(p._1, p._2); Some(s"Die Umfrage mit Id ${p._1} war gestoppt")})
                                  }
        case rsl: Result => {
                                result(id = rsl.id)
                                  .flatMap(r => Some(r))
                              }
        case ls: PollsList.type => {
                                      list(user)
                                   }
        case bgn: Begin => {
                              begin(user, bgn.id).
                                flatMap(b => {sessions = sessions + b;
                                              Some(s"Im Kontext von der Umfrage ${b._2}")})
                           }
        case e: End.type => {
                                end(user).
                                  flatMap(e => {sessions = sessions - e._1 ;
                                    Some(s"Raus aus dem Kontext von der Umfrage ${e._2}")})
                              }
        case addQuiz: AddQuestion => {
          val q = Quiz( addQuiz.quiz,
            addQuiz.questionType.getOrElse(QuizType.OPEN),
            Vector.empty,
            addQuiz.pAnswers)

          addQuestion(userId = user, q = q)
            .flatMap(p => {polls = polls.updated(p._1, p._2); Some(s"Die Frage mit Id ${p._1} war hinzugefuegt")})
        }
        case dltQuiz: DeleteQuestion => {
                                          deleteQuestion(user, dltQuiz.id).
                                            flatMap(p => {polls = polls.updated(p._1, p._2); Some(s"Die Frage mit Id ${dltQuiz.id} war entfernt")})
                                        }
        case vw: View.type => {
                                view(user)
                              }
      }) match {
        case Some(msg: String) => msg
        case None => "Schief gegangen"
      }

    }

}
