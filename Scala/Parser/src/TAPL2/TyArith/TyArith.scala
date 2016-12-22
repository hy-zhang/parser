package TAPL2.TyArith

import TAPL2.Arith.{Nat, Bool}
import TAPL2.Util._
import TAPL2.{Term, Ty}


case object TyBool extends Ty

case object TyNat extends Ty


case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

/* <4> */
object TypedNat {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] extends Nat.Parser[F] {
    lexical.reserved += "Nat"

    val pTypedNatE: (=> F) => PackratParser[Term] = pNatE
    val pTypedNatT: (=> F) => PackratParser[Ty] = l => {
      "Nat" ^^ { _ => TyNat }
    }
  }

}

object TypedBool {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] extends Bool.Parser[F] {
    lexical.reserved += "Bool"

    val pTypedBoolE: (=> F) => PackratParser[Term] = pBoolE
    val pTypedBoolT: (=> F) => PackratParser[Ty] = l => {
      "Bool" ^^ { _ => TyBool }
    }
  }

}

object TyArith {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends TypedBool.Parser[L] with TypedNat.Parser[L] {
    val pTyArithE: (=> L) => PackratParser[Term] =
      l => pTypedBoolE(l) ||| pTypedNatE(l)
    val pTyArithT: (=> L) => PackratParser[Ty] =
      l => pTypedBoolT(l) ||| pTypedNatT(l)
  }

}

object TestTyArith {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new TyArith.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pTyArithE(l), lang.pTyArithT(l))
    }
    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}