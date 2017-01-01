import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by Lazysoul on 2016. 12. 19..
  */
class InterpreterTest {
  @Test def letterTest: Unit = {
    val testText: List[Char] = "a bc defg hijkl".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.Lval, "a", 0), result(0))
    assertEquals(Token(1, TknKind.Lval, "bc", 0), result(1))
    assertEquals(Token(1, TknKind.Lval, "defg", 0), result(2))
    assertEquals(Token(1, TknKind.Lval, "hijkl", 0), result(3))
  }

  @Test def digitTest: Unit = {
    val testText: List[Char] = "1 23 456 7890".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(1, TknKind.IntNum, "", 23), result(1))
    assertEquals(Token(1, TknKind.IntNum, "", 456), result(2))
    assertEquals(Token(1, TknKind.IntNum, "", 7890), result(3))
  }

  @Test def letterAndDigitTest: Unit = {
    val testText: List[Char] = "1 a23 abc d789".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(1, TknKind.Lval, "a23", 0), result(1))
    assertEquals(Token(1, TknKind.Lval, "abc", 0), result(2))
    assertEquals(Token(1, TknKind.Lval, "d789", 0), result(3))
  }

  @Test def stringTest: Unit = {
    val testText: List[Char] = "1 \"a23\" abc d789".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(1, TknKind.String, "a23", 0), result(1))
    assertEquals(Token(1, TknKind.Lval, "abc", 0), result(2))
    assertEquals(Token(1, TknKind.Lval, "d789", 0), result(3))
  }

  @Test def plusTest: Unit = {
    val testText: List[Char] = "asd = 12 + 3 == 20".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.Lval, "asd", 0), result(0))
    assertEquals(Token(1, TknKind.Assign, "=", 0), result(1))
    assertEquals(Token(1, TknKind.IntNum, "", 12), result(2))
    assertEquals(Token(1, TknKind.Plus, "+", 0), result(3))
    assertEquals(Token(1, TknKind.IntNum, "", 3), result(4))
    assertEquals(Token(1, TknKind.Equal, "==", 0), result(5))
    assertEquals(Token(1, TknKind.IntNum, "", 20), result(6))
  }

  @Test def ifTest: Unit = {
    val testText: List[Char] = "asd = 12 + 3 == 20 if".toList
    Main.initctype()
    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.Lval, "asd", 0), result(0))
    assertEquals(Token(1, TknKind.Assign, "=", 0), result(1))
    assertEquals(Token(1, TknKind.IntNum, "", 12), result(2))
    assertEquals(Token(1, TknKind.Plus, "+", 0), result(3))
    assertEquals(Token(1, TknKind.IntNum, "", 3), result(4))
    assertEquals(Token(1, TknKind.Equal, "==", 0), result(5))
    assertEquals(Token(1, TknKind.IntNum, "", 20), result(6))
    assertEquals(Token(1, TknKind.If, "if", 0), result(7))
  }

  @Test def literalExceptionTest(): Unit = {
    val testText: List[Char] = "DS \"dafsafdfa".toList
    Main.initctype()

    try {
      val result = Main.parseToken(testText, List(), 1).reverse
    } catch {
      case e => assertEquals(e.getMessage, "문자열 리터럴을 닫지 않음")
    }
  }

  @Test def funcTest(): Unit = {
    val testText: List[Char] = " test func ".toList
    Main.initctype()

    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.Lval, "test", 0), result(0))
    assertEquals(Token(1, TknKind.Func, "func", 0), result(1))
  }

  @Test def fcallTest(): Unit = {
    val testText: List[Char] = " test func fun1()".toList
    Main.initctype()

    val result = Main.parseToken(testText, List(), 1).reverse
    assertEquals(Token(1, TknKind.Lval, "test", 0), result(0))
    assertEquals(Token(1, TknKind.Func, "func", 0), result(1))
    assertEquals(Token(1, TknKind.Fcall, "fun1", 0), result(2))
    assertEquals(Token(1, TknKind.Lparen, "(", 0), result(3))
    assertEquals(Token(1, TknKind.Rparen, ")", 0), result(4))
  }

  @Test def fcallExceptionTest(): Unit = {
    val testText: List[Char] = " test func fun1(".toList
    Main.initctype()

    try {
      val result = Main.parseToken(testText, List(), 1).reverse
    } catch {
      case e => assertEquals(e.getMessage, "괄호를 닫지 않았습니다.")
    }
  }

}
