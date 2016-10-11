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

  trait Lexer extends Nat.Lexer {
    lexical.reserved += "Nat"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lazy val pNat = new Nat.Parser[E, F]() {}.pE
    lazy val pE: Alg[E, T] => (=> F) => PackratParser[E] = pNat
    lazy val pT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
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

  trait Lexer extends Bool.Lexer {
    lexical.reserved += "Bool"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lazy val pBool = new Bool.Parser[E, F]() {}.pE
    lazy val pE: Alg[E, T] => (=> F) => PackratParser[E] = pBool
    lazy val pT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      "Bool" ^^ { _ => alg.TyBool() }
    }
  }

}

trait TyArithParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TypedBool.Lexer with TypedNat.Lexer {
  val pTypedBoolET = new TypedBool.Parser[E, T, L]() {}
  val pTypedNatET = new TypedNat.Parser[E, T, L]() {}
  val pTyArithLNGE = pTypedBoolET.pE | pTypedNatET.pE
  val pTyArithLNGT = pTypedBoolET.pT | pTypedNatET.pT
}

trait TyArithAlg[E, T] extends TypedBool.Alg[E, T] with TypedNat.Alg[E, T]

trait TyArithPrint extends TyArithAlg[String, String] with TypedBool.Print with TypedNat.Print