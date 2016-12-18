/**
  * Created by Lazysoul on 2016. 12. 18..
  */
object TknKind extends Enumeration {
  type tkn = Value
  val Lparen, Rparen, Plus, Minus, Multi, Divi,
  Assign, Comma, DblQ,
  Equal, NotEq, Less, LessEq, Great, GreatEq,
  If, Else, End, Print, Ident, IntNum,
  String, Letter, Digit, EofTkn, Others, END_list = Value
}
