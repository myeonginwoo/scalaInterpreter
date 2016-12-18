import org.junit.Assert.assertEquals
import org.junit.Test

/**
  * Created by Lazysoul on 2016. 12. 19..
  */
class InterpreterTest {
  @Test def letterTest: Unit = {
    val testText: List[Char] = "a bc defg hijkl".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.Ident, "a", 0), result(0))
    assertEquals(Token(TknKind.Ident, "bc", 0), result(1))
    assertEquals(Token(TknKind.Ident, "defg", 0), result(2))
    assertEquals(Token(TknKind.Ident, "hijkl", 0), result(3))
  }

  @Test def digitTest: Unit = {
    val testText: List[Char] = "1 23 456 7890".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(TknKind.IntNum, "", 23), result(1))
    assertEquals(Token(TknKind.IntNum, "", 456), result(2))
    assertEquals(Token(TknKind.IntNum, "", 7890), result(3))
  }

  @Test def letterAndDigitTest: Unit = {
    val testText: List[Char] = "1 a23 abc d789".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(TknKind.Ident, "a23", 0), result(1))
    assertEquals(Token(TknKind.Ident, "abc", 0), result(2))
    assertEquals(Token(TknKind.Ident, "d789", 0), result(3))
  }

  @Test def stringTest: Unit = {
    val testText: List[Char] = "1 \"a23\" abc d789".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.IntNum, "", 1), result(0))
    assertEquals(Token(TknKind.String, "a23", 0), result(1))
    assertEquals(Token(TknKind.Ident, "abc", 0), result(2))
    assertEquals(Token(TknKind.Ident, "d789", 0), result(3))
  }

  @Test def plusTest: Unit = {
    val testText: List[Char] = "asd = 12 + 3 == 20".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.Ident, "asd", 0), result(0))
    assertEquals(Token(TknKind.Assign, "=", 0), result(1))
    assertEquals(Token(TknKind.IntNum, "", 12), result(2))
    assertEquals(Token(TknKind.Plus, "+", 0), result(3))
    assertEquals(Token(TknKind.IntNum, "", 3), result(4))
    assertEquals(Token(TknKind.Equal, "==", 0), result(5))
    assertEquals(Token(TknKind.IntNum, "", 20), result(6))
  }

  @Test def ifTest: Unit = {
    val testText: List[Char] = "asd = 12 + 3 == 20 if".toList
    Main.initctype()
    val result = Main.parse(testText, List()).reverse
    assertEquals(Token(TknKind.Ident, "asd", 0), result(0))
    assertEquals(Token(TknKind.Assign, "=", 0), result(1))
    assertEquals(Token(TknKind.IntNum, "", 12), result(2))
    assertEquals(Token(TknKind.Plus, "+", 0), result(3))
    assertEquals(Token(TknKind.IntNum, "", 3), result(4))
    assertEquals(Token(TknKind.Equal, "==", 0), result(5))
    assertEquals(Token(TknKind.IntNum, "", 20), result(6))
    assertEquals(Token(TknKind.If, "if", 0), result(7))
  }
}
