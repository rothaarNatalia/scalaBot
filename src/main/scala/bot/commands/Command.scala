package bot.commands

import bot.Response

/**
  * Created by rothaar on 04.02.2018.
  */



trait Command {

  def command: String
  require(!command.isEmpty)

  def params: List[String]

  def execute: Response = {

      ???

  }



}

case class UnknownCommand() extends Command{

  override def command: String = "/unknown_command"

  override def params: List[String] = List()

  override def execute: Response = {}

}
