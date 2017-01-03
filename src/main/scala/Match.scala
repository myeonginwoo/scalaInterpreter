/**
  * Created by Lazysoul on 2017. 1. 3..
  */
object Match {
  def main(args: Array[String]): Unit = {
    println(matchTest("test"))
    println(matchTest(10))
    println(matchTest("stsetasdfa"))
    println(matchTest(64))
    println(matchTest(64.232))
  }

  def matchTest(value: Any) = value match {
    case "test" => "x is test"
    case 10 => s"x is 10"
    case x: String => s"x is String $x"
    case x: Int => s"x is Int $x"
    case _ => "x is not defined"
  }
}
