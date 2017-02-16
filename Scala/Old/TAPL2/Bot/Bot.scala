package TAPL2.Bot

import TAPL2.SimpleBool.Typed
import TAPL2.Util._
import TAPL2.{Term, Ty}

case object TyTop extends Ty

case object TyBot extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term


object Top {

  trait Parser[F <: {val pT : PackratParser[Ty]}] {
    lexical.reserved += "Top"

    val pTopT: (=> F) => PackratParser[Ty] = l => {
      "Top" ^^ { _ => TyTop }
    }
  }

}

object TopBot {

  trait Parser[F <: {val pT : PackratParser[Ty]}] extends Top.Parser[F] {
    lexical.reserved += "Bot"

    private val pBotT: (=> F) => PackratParser[Ty] = l => "Bot" ^^ { _ => TyBot }
    val pTopBotT: (=> F) => PackratParser[Ty] = l => pTopT(l) ||| pBotT(l)
  }

}

object Bot {
  
  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Typed.Parser[L] with TopBot.Parser[L] {
    val pBotE: (=> L) => PackratParser[Term] = l => pTypedE(l)
    val pBotT: (=> L) => PackratParser[Ty] = l => pTypedT(l) ||| pTopBotT(l)
  }

}

object TestBot {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new Bot.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pBotE(l), lang.pBotT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }
}