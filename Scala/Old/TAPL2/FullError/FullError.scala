package TAPL2.FullError

import TAPL2.Util._
import TAPL2.{Term, Ty}
import TAPL2.Bot.Bot
import TAPL2.FullSimple.TypeVar
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

/* <8> */
object Error {

  trait Parser[F <: {val pE : PackratParser[Term]}] {
    lexical.reserved += ("error", "try", "with")

    val pErrorE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      "error" ^^ { _ => TmError } |||
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => TmTry(e1, e2) }
    }
  }

}

object FullError {
  
  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Bot.Parser[L] with TypedBool.Parser[L] with Error.Parser[L] with TypeVar.Parser[L] {
    val pFullErrorE: (=> L) => PackratParser[Term] =
      l => pBotE(l) ||| pTypedBoolE(l) ||| pErrorE(l)
    val pFullErrorT: (=> L) => PackratParser[Ty] =
      l => pBotT(l) ||| pTypedBoolT(l) ||| pTypeVarT(l)
  }

}

object TestFullError {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new FullError.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pFullErrorE(l), lang.pFullErrorT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}