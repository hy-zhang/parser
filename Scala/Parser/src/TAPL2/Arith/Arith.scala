package TAPL2.Arith

import TAPL2.Lib._


case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term


object Bool {

  trait Parser extends EParser {
    lexical.reserved += ("true", "false", "if", "then", "else")
    lexical.delimiters += ("(", ")")

    val pBoolE: PackratParser[Term] = {
      "true" ^^ { _ => TmTrue } |||
        "false" ^^ { _ => TmFalse } |||
        ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => TmIf(e1, e2, e3) } |||
        "(" ~> pE <~ ")"
    }
  }

}

object Nat {

  trait Parser extends EParser {
    lexical.reserved += ("iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")

    val pNatE: PackratParser[Term] = {
      def num(x: Int): Term = x match {
        case 0 => TmZero
        case _ => TmSucc(num(x - 1))
      }

      numericLit ^^ { x => num(x.toInt) } |||
        "succ" ~> pE ^^ TmSucc |||
        "pred" ~> pE ^^ TmPred |||
        "iszero" ~> pE ^^ TmIsZero |||
        "(" ~> pE <~ ")"
    }
  }

}

object Arith {

  trait Parser extends Bool.Parser with Nat.Parser {
    val pArithE: PackratParser[Term] = pNatE ||| pBoolE

    override val pE: PackratParser[Term] = pArithE
  }

}

object TestArith {

  def parseAndPrint(inp: String): Unit = {
    val p = new Arith.Parser {}
    println(parse(p.pE)(inp))
  }

}