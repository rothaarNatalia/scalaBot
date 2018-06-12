package bot.poll

sealed trait Answer[T] {
  def answer: T

  def toString: String
}

object Answer {

  def unapply(value: Answer[_]): Option[_] =
    Some(value.answer)


  implicit class StringAnswer(val a: String) extends Answer[String] {

    override def answer: String = a

  }

  implicit class LongAnswer(val a: Long) extends Answer[Long] {

    override def answer: Long = a

    override def toString = a.toString

  }

  implicit class ListLongAnswer(val a: List[Long]) extends Answer[List[Long]] {

    override def answer: List[Long] = a

    override def toString = a.toString
  }

}