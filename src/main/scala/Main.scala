import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by Lazysoul on 2016. 12. 15..
  */
object Main {

  var ctype: scala.collection.mutable.MutableList[TknKind.Value] =
    scala.collection.mutable.MutableList.fill(256)(TknKind.Others)

  val keyWdTable: List[KeyWord] = List(
    KeyWord("if", TknKind.If), KeyWord("else", TknKind.Else), KeyWord("elif", TknKind.Elif),
    KeyWord("while", TknKind.While),
    KeyWord("end", TknKind.End), KeyWord("print", TknKind.Print), KeyWord("println", TknKind.Println),
    KeyWord("func", TknKind.Func), KeyWord("exit", TknKind.Exit),
    KeyWord("(", TknKind.Lparen), KeyWord(")", TknKind.Rparen),
    KeyWord("+", TknKind.Plus), KeyWord("-", TknKind.Minus),
    KeyWord("*", TknKind.Multi), KeyWord("/", TknKind.Divi), KeyWord("=", TknKind.Assign),
    KeyWord(",", TknKind.Comma), KeyWord("==", TknKind.Equal), KeyWord("!=", TknKind.NotEq),
    KeyWord("<", TknKind.Less), KeyWord("<=", TknKind.LessEq), KeyWord(">", TknKind.Great),
    KeyWord(">=", TknKind.GreatEq), KeyWord("", TknKind.END_list)
  )

  var integerConstantTable: scala.collection.mutable.MutableList[Int] = scala.collection.mutable.MutableList()
  var stringConstantTable: scala.collection.mutable.MutableList[String] = scala.collection.mutable.MutableList()
  var globalSymbolTable: scala.collection.mutable.MutableList[Symbol] = scala.collection.mutable.MutableList()
  var localSymbolTable: scala.collection.mutable.MutableList[Symbol] = scala.collection.mutable.MutableList()

  def main(args: Array[String]): Unit = {
    initctype()

    val source = Source.fromFile("files/test3.txt").toList

    println(s"source : \n${source.mkString("")}")
    println("---------------------------")
    println(s"result : \n${
      parse(source, List(), 1)
        .reverse
        .map(convertSymbol)
        .mkString("\n")
    }")

  }

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
    ctype('$') = TknKind.Letter
    ctype('=') = TknKind.Assign
    ctype(',') = TknKind.Comma
    ctype('"') = TknKind.DblQ
  }

  @tailrec
  def parse(list: List[Char], acc: List[Token], lineNum: Int): List[Token] = list match {
    case List() => acc
    case ' ' :: _ => parse(list.tail, acc, lineNum)
    case '\n' :: _ => parse(list.tail, acc, lineNum + 1)
    case head :: _ => ctype(head) match {
      case TknKind.Letter =>
        val letters = list
          .takeWhile(char => (ctype(char) == TknKind.Letter)
            || ctype(char) == TknKind.Digit)
          .mkString("")
        if (list.startsWith(letters.toString + "()")) {
          parse(list.drop(letters.length), Token(lineNum, getTkKind(letters.toString + "()"), letters.mkString("")) :: acc, lineNum)
        } else {
          parse(list.drop(letters.length), Token(lineNum, getTkKind(letters), letters.mkString("")) :: acc, lineNum)
        }
      case TknKind.Digit =>
        val digits = list.takeWhile(ctype(_) == TknKind.Digit).mkString("")
        //        println(s"num = ${num}")
        parse(list.drop(digits.length), Token(lineNum, getTkKind(digits), "", digits.toInt) :: acc, lineNum)
      case TknKind.DblQ =>
        val literal = list.tail.takeWhile(ctype(_) != TknKind.DblQ).mkString("")
        //        println(s"literalLength = ${literal.length}")
        if (literal.length + 1 == list.length) {
          throw new IllegalArgumentException("문자열 리터럴을 닫지 않음")
        }
        //        println(s"literalLength = ${literal.length}")
        parse(list.drop(literal.length + 2), Token(lineNum, TknKind.String, literal) :: acc, lineNum)
      case TknKind.Lparen =>
        parse(list.drop(1), Token(lineNum, TknKind.Lparen, "(") :: acc, lineNum)
      case TknKind.Rparen =>
        parse(list.drop(1), Token(lineNum, TknKind.Rparen, ")") :: acc, lineNum)
      case _ =>
        val value = list
          .takeWhile(char => char != ' '
            && ctype(char) != TknKind.Letter
            && ctype(char) != TknKind.Digit
          )
          .mkString("")
        parse(list.drop(value.length), Token(lineNum, getTkKind(value), value) :: acc, lineNum)
    }
  }

  def getTkKind(value: String): TknKind.Value = {
    keyWdTable.find(value == _.keyName) match {
      case None => ctype(value.head) match {
        case TknKind.Letter =>
          if ('$' == value.head) TknKind.Gval
          else if (value endsWith "()") TknKind.Fcall
          else if (value endsWith "(") throw new IllegalArgumentException("괄호를 닫지 않았습니다.")
          else if (value endsWith ")") throw new IllegalArgumentException("괄호를 닫지 않았습니다.")
          else TknKind.Lval
        case TknKind.Digit => TknKind.IntNum
        case _ =>
          println(s"others : $value")
          TknKind.Others
      }
      case Some(tkKind) => tkKind.keyKind
    }
  }

  def convertSymbol(tk: Token): String = tk.kind match {
    case TknKind.Fcall =>
      val symbol = Symbol(globalSymbolTable.size, tk.text, SymbolKind.fncId)
      globalSymbolTable += symbol
      s"[${tk.kind.toString}][${symbol.index}]"
    case TknKind.Gval =>
      val gSymbol = globalSymbolTable.find(_.name == tk.text) match {
        case Some(x) => x
        case None =>
          val symbol = Symbol(globalSymbolTable.size, tk.text, SymbolKind.varId)
          globalSymbolTable += symbol
          symbol
      }
      s"[${tk.kind.toString}][${gSymbol.index}]"
    case TknKind.Lval =>
      val lSymbol = localSymbolTable.find(_.name == tk.text) match {
        case Some(x) => x
        case None =>
          val symbol = Symbol(localSymbolTable.size, tk.text, SymbolKind.varId)
          localSymbolTable += symbol
          symbol
      }
      s"[${tk.kind.toString}][${lSymbol.index}]"
    case TknKind.String =>
      val str = stringConstantTable.find(_ == tk.text) match {
        case Some(x) => x
        case None =>
          stringConstantTable += tk.text
          tk.text
      }
      s"[${tk.kind.toString}][${stringConstantTable.indexOf(str)}]"
    case TknKind.IntNum =>
      val intVal = integerConstantTable.find(_ == tk.intVal) match {
        case Some(x) => x
        case None =>
          integerConstantTable += tk.intVal
          tk.intVal
      }
      s"[${tk.kind.toString}][${integerConstantTable.indexOf(intVal)}]"
    case _ => s"[${tk.kind.toString}]"
  }
}
