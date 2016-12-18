import scala.io.Source

/**
  * Created by Lazysoul on 2016. 12. 15..
  */
object Main {

  var ctype: scala.collection.mutable.MutableList[TknKind.Value] =
    scala.collection.mutable.MutableList.fill(256)(TknKind.Others)
  var token: Token = _
  var ch = ' '
  var c = 0
  var ch0, num = 0

  def initctype(): Unit = {
    for (i <- '0' to '9') {
      ctype(i) = TknKind.Digit
    }

    for (i <- 'A' to 'Z') {
      ctype(i) = TknKind.Letter
    }

    for (i <- 'a' to 'z') {
      ctype(i) = TknKind.Letter
    }

    ctype('(') = TknKind.Lparen
    ctype(')') = TknKind.Rparen
    ctype('<') = TknKind.Less
    ctype('>') = TknKind.Great
    ctype('+') = TknKind.Plus
    ctype('-') = TknKind.Minus
    ctype('*') = TknKind.Multi
    ctype('/') = TknKind.Divi
    ctype('_') = TknKind.Letter
    ctype('=') = TknKind.Assign
    ctype(',') = TknKind.Comma
    ctype('\"') = TknKind.DblQ
  }

  val keyWdTabl: List[KeyWord] = List(KeyWord("if", TknKind.If), KeyWord("else", TknKind.Else),
    KeyWord("end", TknKind.End), KeyWord("print", TknKind.Print), KeyWord("(", TknKind.Lparen),
    KeyWord(")", TknKind.Rparen), KeyWord("+", TknKind.Plus), KeyWord("-", TknKind.Minus),
    KeyWord("*", TknKind.Multi), KeyWord("/", TknKind.Divi), KeyWord("=", TknKind.Assign),
    KeyWord(",", TknKind.Comma), KeyWord("==", TknKind.Equal), KeyWord("!=", TknKind.NotEq),
    KeyWord("<", TknKind.Less), KeyWord("<=", TknKind.LessEq), KeyWord(">", TknKind.Great),
    KeyWord(">=", TknKind.GreatEq), KeyWord("", TknKind.END_list)
  )

  def main(args: Array[String]): Unit = {
    println("test   kind intVal")
    initctype()

    val source = Source.fromFile("files/test.txt").toList

    println(s"source : ${source.mkString("")}")

    println(s"result : ${parse(source, List()).reverse.mkString("\n")}")
  }

  def parse(list: List[Char], acc: List[Token]): List[Token] = list match {
    case List() => acc
    case ' ' :: _ | '\n' :: _ => parse(list.tail, acc)
    case head :: _ => ctype(head) match {
      case TknKind.Letter =>
        val letters = list.takeWhile(char => (ctype(char) == TknKind.Letter) || (ctype(char) == TknKind.Digit))
          .mkString("")
        //        println(s"ident = ${letters}")
        parse(list.drop(letters.length), Token(getTkKind(letters), letters.mkString("")) :: acc)
      case TknKind.Digit =>
        val digits = list.takeWhile(ctype(_) == TknKind.Digit).mkString("")
        //        println(s"num = ${num}")
        parse(list.drop(digits.length), Token(getTkKind(digits), "", digits.toInt) :: acc)
      case TknKind.DblQ =>
        val literal = list.tail.takeWhile(ctype(_) != TknKind.DblQ).mkString("")
        println(s"literalLength = ${literal.length}")

        if (literal.length + 1 == list.length) {
          throw new IllegalArgumentException("문자열 리터럴을 닫지 않음")
        }
        //        println(s"literalLength = ${literal.length}")
        parse(list.drop(literal.length + 2), Token(TknKind.String, literal, 0) :: acc)
      case _ => {
        val value = list.takeWhile(char => char != ' ' && ctype(char) != TknKind.Letter && ctype(char) != TknKind.Digit).mkString("")
        //        println(s"value : ${value}")
        parse(list.drop(value.length), Token(getTkKind(value), value, 0) :: acc)
      }
    }
  }

  def getTkKind(value: String): TknKind.Value = {
    keyWdTabl.find(value == _.keyName) match {
      case None => ctype(value.head) match {
        case TknKind.Letter => TknKind.Ident
        case TknKind.Digit => TknKind.IntNum
        case _ => TknKind.Others
      }
      case Some(tkKind) => tkKind.keyKind
    }
  }
}
