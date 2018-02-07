package bot

import bot.commands.{Command, UnknownCommand}
import play.api.libs.json.{JsArray, JsValue}

/**
  * Created by rothaar on 04.02.2018.
  */
object Parser {

  def parse(entity: JsValue): Command = {

    val entities = (entity \"entities").asOpt[JsArray].getOrElse(JsArray()).value

    if(entities.length == 0)
      UnknownCommand()

    val isCommand = (entities find (e => (e \ "type").asOpt[String].getOrElse("None") == "bot_command")) != None

    if(isCommand)
      UnknownCommand()

    val commandSplitted = ((entity \ "text").as[String]) replace("\r\n", " ") replaceAll("(\\(\\()", "{") replaceAll("(\\)\\))", "}") trim

    val cmd = commandSplitted.takeWhile(!_.isWhitespace)

    val paramsString = commandSplitted.dropWhile(!_.isWhitespace)
    val pars = "\\(.+?\\)".r findAllIn(paramsString) toList

    new Command {
      override def params: List[String] = pars
      override def command: String = cmd
    }
  }

}
