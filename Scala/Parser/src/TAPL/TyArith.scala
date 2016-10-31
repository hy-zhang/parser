package TAPL

import Util._

/* <4> */
object TypedNat {

  trait Alg[E, T] extends Nat.Alg[E] {
    def TyNat(): T
  }

  trait Print extends Alg[String, String] with Nat.Print {
    def TyNat() = "Nat"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends Nat.Parser[E, F] {
    lexical.reserved += "Nat"

    val pTypedNatE: Alg[E, T] => (=> F) => PackratParser[E] = pNatE
    val pTypedNatT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      "Nat" ^^ { _ => alg.TyNat() }
    }
  }

}

object TypedBool {

  trait Alg[E, T] extends Bool.Alg[E] {
    def TyBool(): T
  }

  trait Print extends Alg[String, String] with Bool.Print {
    def TyBool() = "Bool"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends Bool.Parser[E, F] {
    lexical.reserved += "Bool"

    val pTypedBoolE: Alg[E, T] => (=> F) => PackratParser[E] = pBoolE
    val pTypedBoolT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      "Bool" ^^ { _ => alg.TyBool() }
    }
  }

}

object TyArith {

  trait Alg[E, T] extends TypedBool.Alg[E, T] with TypedNat.Alg[E, T]

  trait Print extends Alg[String, String] with TypedBool.Print with TypedNat.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends TypedBool.Parser[E, T, L] with TypedNat.Parser[E, T, L] {
    val pTyArithE = pTypedBoolE | pTypedNatE
    val pTyArithT = pTypedBoolT | pTypedNatT
  }

}

object TestTyArith {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: TyArith.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new TyArith.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pTyArithE(alg)(l), lang.pTyArithT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new TyArith.Print {})

}