package TAPL2.TyArith

import TAPL2.Lib._
import TAPL2.Arith.{Nat, Bool}


case object TyBool extends Ty

case object TyNat extends Ty


case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term


object TypedNat {

  trait Parser extends Nat.Parser {
    lexical.reserved += "Nat"

    val pTypedNatE: PackratParser[Term] = pNatE
    val pTypedNatT: PackratParser[Ty] = "Nat" ^^ { _ => TyNat }
  }

}

object TypedBool {

  trait Parser extends Bool.Parser {
    lexical.reserved += "Bool"

    val pTypedBoolE: PackratParser[Term] = pBoolE
    val pTypedBoolT: PackratParser[Ty] = "Bool" ^^ { _ => TyBool }
  }

}


object TyArith {

  trait Parser extends ETParser with TypedBool.Parser with TypedNat.Parser {

    val pTyArithE: PackratParser[Term] = pTypedBoolE ||| pTypedNatE
    val pTyArithT: PackratParser[Ty] = pTypedBoolT ||| pTypedNatT

    override val pE: PackratParser[Term] = pTyArithE

    override val pT: PackratParser[Ty] = pTyArithT
  }

}

object TestTyArith {
  def parseAndPrint(inp: String): Unit = {
    val p = new TyArith.Parser {}
    println(parse(p.pE)(inp))
  }
}