/**
  * Created by rothaar on 04.02.2018.
  */
package object bot {

  type Answer = Any
  type Response = String
  type UserId = String
  type Result = (Answer, Int, Option[List[UserId]])

  object Responses{
    val OK = "Ok"
    val Denied = "Denied"
    val NotExist = "Doesn't exist"
  }

}
