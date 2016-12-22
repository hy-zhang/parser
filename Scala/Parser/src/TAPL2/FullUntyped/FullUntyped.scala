package TAPL2.FullUntyped

import TAPL2.Arith.Arith
import TAPL2.Untyped.Untyped
import TAPL2.Util._
import TAPL2.Term


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

/* <3> */
object Record {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

    val pRecordE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      List(
        "{" ~> repsep(lcid ~ ("=" ~> e) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TmRecord,
        e ~ ("." ~> lcid) ^^ { case e ~ x => TmProj(e, x) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Let {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.reserved += ("let", "in")
    lexical.delimiters += "="

    val pLetE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      ("let" ~> lcid) ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case x ~ e1 ~ e2 => TmLet(x, e1, e2) }
    }
  }

}

object FloatString {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.delimiters += "*"

    val pFloatStringE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      //chainl1(e, "*" ^^^ { (e1, e2) => TmTimesfloat(e1, e2) }) |||
      stringLit ^^ TmString
    }
  }

}

object FullUntyped {

  trait Parser[L <: {val pE : PackratParser[Term]}]
    extends Arith.Parser[L] with Untyped.Parser[L] with Record.Parser[L]
      with FloatString.Parser[L] with Let.Parser[L] {
    val pFullUntypedE: (=> L) => PackratParser[Term] =
      l => pArithE(l) ||| pUntypedE(l) ||| pRecordE(l) ||| pFloatStringE(l) ||| pLetE(l)
  }

}

object TestFullUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term]): List[Term] = {
      val lang = new FullUntyped.Parser[List[Term]] {}
      new List[Term](lang.pFullUntypedE(l))
    }
    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}