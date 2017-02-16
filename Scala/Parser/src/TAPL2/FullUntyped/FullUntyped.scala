package TAPL2.FullUntyped

import TAPL2.Lib._
import TAPL2.Arith.Arith
import TAPL2.Untyped.Untyped


case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

case class TmString(s: String) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmLet(l: String, t1: Term, t2: Term) extends Term

case class TmTimesFloat(a: Term, b: Term) extends Term


object Record {

  trait Parser extends EParser {
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

    val pRecordE: PackratParser[Term] =
      "{" ~> repsep(lcid ~ ("=" ~> pE) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TmRecord |||
        pE ~ ("." ~> lcid) ^^ { case e ~ x => TmProj(e, x) }
  }

}

object Let {

  trait Parser extends EParser {
    lexical.reserved += ("let", "in")
    lexical.delimiters += "="

    val pLetE: PackratParser[Term] =
      ("let" ~> lcid) ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ { case x ~ e1 ~ e2 => TmLet(x, e1, e2) }
  }

}

object FloatString {

  trait Parser extends EParser {
    lexical.delimiters += "*"

    val pFloatStringE: PackratParser[Term] =
      //chainl1(pE, "*" ^^^ { (e1: Term, e2: Term) => TmTimesfloat(e1, e2) }) |||
        stringLit ^^ TmString
  }

}

object FullUntyped {

  trait Parser extends Arith.Parser with Untyped.Parser with Record.Parser
    with FloatString.Parser with Let.Parser {

    val pFullUntypedE: PackratParser[Term] = pArithE ||| pUntypedE ||| pRecordE ||| pFloatStringE ||| pLetE

    override val pE: PackratParser[Term] = pFullUntypedE
  }

}

object TestFullUntyped {
  def parseAndPrint(inp: String): Unit = {
    val p = new FullUntyped.Parser {}
    println(parse(p.pE)(inp))
  }
}