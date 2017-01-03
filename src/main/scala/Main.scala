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

    val source = Source.fromFile("files/test2.txt").toList

    val tokens = parseToken(source, List(), 1)
    //    val tokens = parseToken(source, List(), 1).groupBy(_.line).toSeq.sortBy(_._1)
    println(s"source : \n${source.mkString("")}")
    println("---------------------------")
    println(s"result : \n${
      convert(tokens, List())
        .groupBy(_.line)
        .toSeq
        .sortBy(_._1)
        .map(sortiedMap => sortiedMap
          ._2
          .map(_.code)
          .mkString(""))
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

  //  @tailrec
  def parseToken(list: List[Char], acc: List[Token], lineNum: Int): List[Token] = list match {
    case List() => acc
    case ' ' :: _ => parseToken(list.tail, acc, lineNum)
    case '\n' :: _ => parseToken(list.tail, acc, lineNum + 1)
    case head :: _ => ctype(head) match {
      case TknKind.Letter =>
        val letters = list
          .takeWhile(char => (ctype(char) == TknKind.Letter)
            || ctype(char) == TknKind.Digit)
          .mkString("")
        if (list.startsWith(letters + "()")) {
          parseToken(list.drop(letters.length), acc ::: Token(lineNum, getTkKind(letters.toString + "()"), letters.mkString("")) :: List(), lineNum)
        } else if (list.startsWith("func")) {
          parseToken(list.drop(list.indexOf('(')), acc ::: Token(lineNum, getTkKind(letters), letters.mkString("")) :: List(), lineNum)
        } else {
          parseToken(list.drop(letters.length), acc ::: Token(lineNum, getTkKind(letters), letters.mkString("")) :: List(), lineNum)
        }
      case TknKind.Digit =>
        val digits = list.takeWhile(ctype(_) == TknKind.Digit).mkString("")
        parseToken(list.drop(digits.length), acc ::: Token(lineNum, getTkKind(digits), "", digits.toInt) :: List(), lineNum)
      case TknKind.DblQ =>
        val literal = list.tail.takeWhile(ctype(_) != TknKind.DblQ).mkString("")
        if (literal.length + 1 == list.length) {
          throw new IllegalArgumentException("문자열 리터럴을 닫지 않음")
        }
        parseToken(list.drop(literal.length + 2), acc ::: Token(lineNum, TknKind.String, literal) :: List(), lineNum)
      case TknKind.Lparen =>
        parseToken(list.drop(1), acc ::: Token(lineNum, TknKind.Lparen, "(") :: List(), lineNum)
      case TknKind.Rparen =>
        parseToken(list.drop(1), acc ::: Token(lineNum, TknKind.Rparen, ")") :: List(), lineNum)
      case _ =>
        val value = list
          .takeWhile(char => char != ' '
            && ctype(char) != TknKind.Letter
            && ctype(char) != TknKind.Digit
          )
          .mkString("")
        parseToken(list.drop(value.length), acc ::: Token(lineNum, getTkKind(value), value) :: List(), lineNum)
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

  //  @tailrec
  def convert(list: List[Token], acc: List[Token]): List[Token] = list match {
    case List() => acc
    case token :: tail => token.kind match {
      case TknKind.Fcall =>
        val symbol = Symbol(globalSymbolTable.size, token, SymbolKind.fncId)
        globalSymbolTable += symbol
        token.code = symbol.toCode()
        convert(tail, acc ::: token :: List())
      case TknKind.Gval =>
        val gSymbol = globalSymbolTable.find(_.token.text == token.text) match {
          case Some(x) => x
          case None =>
            val symbol = Symbol(globalSymbolTable.size, token, SymbolKind.varId)
            globalSymbolTable += symbol
            symbol
        }
        token.code = gSymbol.toCode()
        convert(tail, acc ::: token :: List())
      case TknKind.Lval =>
        val lSymbol = localSymbolTable.find(_.token.text == token.text) match {
          case Some(x) => x
          case None =>
            val symbol = Symbol(localSymbolTable.size, token, SymbolKind.varId)
            localSymbolTable += symbol
            symbol
        }
        token.code = lSymbol.toCode()
        convert(tail, acc ::: token :: List())
      case TknKind.String =>
        val str = stringConstantTable.find(_ == token.text) match {
          case Some(x) => x
          case None =>
            stringConstantTable += token.text
            token.text
        }
        token.code = s"[${token.kind.toString}][${stringConstantTable.indexOf(str)}]"
        convert(tail, acc ::: token :: List())
      case TknKind.IntNum =>
        val intVal = integerConstantTable.find(_ == token.intVal) match {
          case Some(x) => x
          case None =>
            integerConstantTable += token.intVal
            token.intVal
        }
        token.code = s"[${token.kind.toString}][${integerConstantTable.indexOf(intVal)}]"
        convert(tail, acc ::: token :: List())
      case TknKind.While | TknKind.If | TknKind.Elif | TknKind.Else | TknKind.For | TknKind.Func =>
        val blockResult = convert(tail, List())
        println(s"blockResult : $blockResult")
        token.code = s"[${token.kind.toString}][${blockResult.last.line}]"
        convert(tail.drop(blockResult.size), acc ::: token :: blockResult)
      case TknKind.End =>
        token.code = s"[${token.kind.toString}]"
        acc ::: token :: List()
      case _ =>
        token.code = s"[${token.kind.toString}]"
        convert(tail, acc ::: token :: List())
    }
  }

  def convertBlockSet(list: List[Token], acc: List[Token]): ((Int, List[Token]), List[Token]) = list.head.kind match {
    case TknKind.End =>
      ((list.head.line, list.tail), acc ::: list.head :: List())
    case _ =>
      convertBlockSet(convert(list, acc), acc ::: list.head :: List())
  }


  def convertBlock(): Unit = {

  }
}
