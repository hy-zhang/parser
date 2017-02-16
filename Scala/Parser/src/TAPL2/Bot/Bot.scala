package TAPL2.Bot

import TAPL2.Lib._
import TAPL2.SimpleBool.Typed


case object TyTop extends Ty

case object TyBot extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term


object Top {

  trait Parser {
    lexical.reserved += "Top"

    val pTopT: PackratParser[Ty] = "Top" ^^ { _ => TyTop }
  }

}

object TopBot {

  trait Parser extends Top.Parser {
    lexical.reserved += "Bot"

    val pTopBotT: PackratParser[Ty] = pTopT ||| "Bot" ^^ { _ => TyBot }
  }

}

object Bot {

  trait Parser extends Typed.Parser with TopBot.Parser {

    val pBotE: PackratParser[Term] = pTypedE
    val pBotT: PackratParser[Ty] = pTypedT ||| pTopBotT

    override val pE: PackratParser[Term] = pBotE

    override val pT: PackratParser[Ty] = pBotT
  }

}

object TestBot {
  def parseAndPrint(inp: String): Unit = {
    val p = new Bot.Parser {}
    println(parse(p.pE)(inp))
  }

}