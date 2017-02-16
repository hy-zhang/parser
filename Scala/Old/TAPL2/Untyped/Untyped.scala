package TAPL2.Untyped

import TAPL2.Util._
import TAPL2.Term


case class TmVar(i: String) extends Term

case class TmAbs(v: String, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

object UntypedAbs {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.delimiters += ("\\", ".", "(", ")")

    val pUntypedAbsE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => TmAbs(x, e0) } ||| "(" ~> e <~ ")"
    }
  }

}

object VarApp {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.delimiters += ("(", ")")

    val pVarAppE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      List(
        lcid ^^ TmVar,
        e ~ e ^^ { case e1 ~ e2 => TmApp(e1, e2) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Untyped {

  trait Parser[L <: {val pE : PackratParser[Term]}]
    extends UntypedAbs.Parser[L] with VarApp.Parser[L] {
    val pUntypedE: (=> L) => PackratParser[Term] = l => pUntypedAbsE(l) ||| pVarAppE(l)
  }

}

object TestUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term]): List[Term] = {
      val lang = new Untyped.Parser[List[Term]] {}
      new List[Term](lang.pUntypedE(l))
    }
    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}