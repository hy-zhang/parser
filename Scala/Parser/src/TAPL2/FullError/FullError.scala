package TAPL2.FullError

import TAPL2.Lib._
import TAPL2.FullSimple.TypeVar
import TAPL2.Bot.Bot
import TAPL2.TyArith.TypedBool


case class TyVar(i: String) extends Ty

case object TyTop extends Ty

case object TyBot extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyBool extends Ty

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmError extends Term

case class TmTry(t1: Term, t2: Term) extends Term


object Error {

  trait Parser extends EParser {
    lexical.reserved += ("error", "try", "with")
    
    val pErrorE: PackratParser[Term] =
      "error" ^^ { _ => TmError } |||
        "try" ~> pE ~ ("with" ~> pE) ^^ { case e1 ~ e2 => TmTry(e1, e2) }
  }

}

object FullError {

  trait Parser extends Bot.Parser with TypedBool.Parser with Error.Parser with TypeVar.Parser {

    val pFullErrorE: PackratParser[Term] = pBotE ||| pTypedBoolE ||| pErrorE
    val pFullErrorT: PackratParser[Ty] = pBotT ||| pTypedBoolT ||| pTypeVarT

    override val pE: PackratParser[Term] = pFullErrorE
    override val pT: PackratParser[Ty] = pFullErrorT
  }

}

object TestFullError {
  def parseAndPrint(inp: String): Unit = {
    val p = new FullError.Parser {}
    println(parse(p.pE)(inp))
  }

}