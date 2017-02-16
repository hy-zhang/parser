package TAPL2.RcdSubBot

import TAPL2.Util._
import TAPL2.{Term, Ty}
import TAPL2.Bot.Bot
import TAPL2.FullSimple.TypedRecord

case object TyTop extends Ty

case object TyBot extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

/* <9> */
object RcdSubBot {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Bot.Parser[L] with TypedRecord.Parser[L] {
    val pRcdSubBotE: (=> L) => PackratParser[Term] =
      l => pBotE(l) ||| pTypedRecordE(l)
    val pRcdSubBotT: (=> L) => PackratParser[Ty] =
      l => pBotT(l) ||| pTypedRecordT(l)
  }

}

object TestRcdSubBot {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new RcdSubBot.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pRcdSubBotE(l), lang.pRcdSubBotT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}