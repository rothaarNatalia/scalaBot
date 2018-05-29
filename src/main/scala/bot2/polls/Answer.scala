package bot2.polls

sealed trait Answer[T] {
  def answer: T

  def toString: String
}

object Answer {

  implicit class StringAnswer(val a: String) extends Answer[String] {

    override def answer: String = a

  }

  implicit class IntAnswer(val a: Long) extends Answer[Long] {

    override def answer: Long = a

    override def toString = a.toString
  }

  implicit class SeqLongAnswer(val a: Seq[Long]) extends Answer[Seq[Long]] {

    override def answer: Seq[Long] = a

    override def toString = a.toString
  }

}