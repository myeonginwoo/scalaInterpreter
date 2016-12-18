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
}
