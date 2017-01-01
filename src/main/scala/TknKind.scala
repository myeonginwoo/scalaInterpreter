/**
  * Created by Lazysoul on 2016. 12. 18..
  */
object TknKind extends Enumeration {
  //  type tkn = Value
  val Lparen, Rparen, Plus, Minus, Multi, Divi,
  Assign, Comma, DblQ, Func, Fcall,
  Equal, NotEq, Less, LessEq, Great, GreatEq,
  If, Else, Elif, While, For, End, Print, Println,
  Gval, Lval, IntNum, Exit,
  String, Letter, Digit, EofTkn, Others, END_list = Value
}
