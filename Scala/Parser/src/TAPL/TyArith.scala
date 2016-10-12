package TAPL

import Util._

/* <4> */
object TypedNat {

  trait NatTypeAlg[T] {
    def TyNat(): T
  }

  trait Alg[E, T] extends Nat.Alg[E] with NatTypeAlg[T]

  trait Print extends Alg[String, String] with Nat.Print {
    def TyNat() = "Nat"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends Nat.Parser[E, F] {
    lexical.reserved += "Nat"

    lazy val pTypedNatE: Alg[E, T] => (=> F) => PackratParser[E] = pNatE
    lazy val pTypedNatT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      "Nat" ^^ { _ => alg.TyNat() }
    }
  }

}

object TypedBool {

  trait BoolTypeAlg[T] {
    def TyBool(): T
  }

  trait Alg[E, T] extends Bool.Alg[E] with BoolTypeAlg[T]

  trait Print extends Alg[String, String] with Bool.Print {
    def TyBool() = "Bool"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends Bool.Parser[E, F] {
    lexical.reserved += "Bool"

    lazy val pTypedBoolE: Alg[E, T] => (=> F) => PackratParser[E] = pBoolE
    lazy val pTypedBoolT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      "Bool" ^^ { _ => alg.TyBool() }
    }
  }

}

trait TyArithParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TypedBool.Parser[E, T, L] with TypedNat.Parser[E, T, L] {
  val pTyArithLNGE = pTypedBoolE | pTypedNatE
  val pTyArithLNGT = pTypedBoolT | pTypedNatT
}

trait TyArithAlg[E, T] extends TypedBool.Alg[E, T] with TypedNat.Alg[E, T]

trait TyArithPrint extends TyArithAlg[String, String] with TypedBool.Print with TypedNat.Print