package TAPL2.Arith

import TAPL2.Util._
import TAPL2.Term


case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term


object Bool {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.reserved += ("true", "false", "if", "then", "else")
    lexical.delimiters += ("(", ")")

    val pBoolE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      List(
        "true" ^^ { _ => TmTrue },
        "false" ^^ { _ => TmFalse },
        ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => TmIf(e1, e2, e3) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Nat {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.reserved += ("iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")

    val pNatE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      def num(x: Int): Term = if (x == 0) TmZero else TmSucc(num(x - 1))

      List(
        numericLit ^^ { x => num(x.toInt) },
        "succ" ~> e ^^ TmSucc,
        "pred" ~> e ^^ TmPred,
        "iszero" ~> e ^^ TmIsZero,
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Arith {

  trait Parser[L <: {val pE : PackratParser[Term]}] extends Bool.Parser[L] with Nat.Parser[L] {
    val pArithE: (=> L) => PackratParser[Term] = l => pBoolE(l) ||| pNatE(l)
    // we cannot use pE, such a name has incompatible types when overridden. is there a solution?
  }

}

object TestArith {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term]): List[Term] = {
      val lang = new Arith.Parser[List[Term]] {}
      new List[Term](lang.pArithE(l))
    }
    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}