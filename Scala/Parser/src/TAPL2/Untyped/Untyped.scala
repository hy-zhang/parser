package TAPL2.Untyped

import TAPL2.Lib._


case class TmVar(i: String) extends Term

case class TmAbs(v: String, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term


object UntypedAbs {

  trait Parser extends EParser {
    lexical.delimiters += ("\\", ".")

    val pUntypedAbsE: PackratParser[Term] =
      ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => TmAbs(x, e0) }
  }

}

object VarApp {

  trait Parser extends EParser {
    lexical.delimiters += ("(", ")")

    val pVarAppE: PackratParser[Term] = {
      lcid ^^ TmVar |||
        pE ~ pE ^^ { case e1 ~ e2 => TmApp(e1, e2) } |||
        "(" ~> pE <~ ")"
    }
  }

}

object Untyped {

  trait Parser extends UntypedAbs.Parser with VarApp.Parser {
    val pUntypedE: PackratParser[Term] = pUntypedAbsE ||| pVarAppE

    override val pE: PackratParser[Term] = pUntypedE
  }

}

object TestUntyped {
  def parseAndPrint(inp: String): Unit = {
    val p = new Untyped.Parser {}
    println(parse(p.pE)(inp))
  }
}