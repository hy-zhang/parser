package TAPL2.FullSub

import TAPL2.FullSimple.Simple
import TAPL2.Util._
import TAPL2.{Term, Ty}
import TAPL2.Bot.Top

object FullSub {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Simple.Parser[L] with Top.Parser[L] {
    val pFullSubE: (=> L) => PackratParser[Term] = pSimpleE
    val pFullSubT: (=> L) => PackratParser[Ty] =
      l => pSimpleT(l) ||| pTopT(l)
  }

}

object TestFullSub {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new FullSub.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pFullSubE(l), lang.pFullSubT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}
