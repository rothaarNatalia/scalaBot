package bot2.polls

sealed trait Answer[T] {
  def answer: T

  def toString: String
}

object Answer {

  implicit class StringAnswer(val a: String) extends Answer[String] {

    override def answer: String = a

  }

  implicit class IntAnswer(val a: Int) extends Answer[Int] {

    override def answer: Int = a

    override def toString = a.toString
  }

}