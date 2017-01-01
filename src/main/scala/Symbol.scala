/**
  * Created by Lazysoul on 2016. 12. 31..
  */
case class Symbol(index: Int, token: Token, symbolType: SymbolKind.Value) {
  def toCode(): String = {
    symbolType match {
      case SymbolKind.fncId =>
        s"[${token.kind.toString}][$index]"
      case SymbolKind.varId =>
        s"[${token.kind.toString}][$index]"
    }
  }
}
