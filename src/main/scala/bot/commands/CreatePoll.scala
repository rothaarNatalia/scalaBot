package bot.commands

import java.util.UUID

import bot.pools.Visibility
import bot.Response

/**
  * Created by rothaar on 04.02.2018.
  */

case class CreatePoll(cmd: String, pars: List[String]) extends Command {

  override def command  = cmd
  override def params: List[String] = pars

  private def title = params(0) replace ('{', '(') replace ('}', ')')
  require(!title.isEmpty)

  private def isAnonymous: Boolean = if (params.length > 1) params(1) toBoolean else true

  private def visibility = if (params.length > 2) Visibility.withName(params(2) toUpperCase)  else Visibility.AFTERSTOP

  private def dateFrom = None

  private def dateTo = None

  override def execute(): Response ={


    val uuid = UUID.randomUUID() toString

    uuid
  }

}



