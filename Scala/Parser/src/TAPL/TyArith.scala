package TAPL

import Util._

/* <4> */
object NatType {

  trait Alg[T] {
    def nat(): T
  }

  trait Print extends Alg[String] {
    def nat() = "Nat"
  }

  trait Lexer {
    lexical.reserved += "Nat"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Nat" ^^ { _ => alg.nat() }
    }
  }

}

object BoolType {

  trait Alg[T] {
    def bool(): T
  }

  trait Print extends Alg[String] {
    def bool() = "Bool"
  }

  trait Lexer {
    lexical.reserved += "Bool"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Bool" ^^ { _ => alg.bool() }
    }
  }

}

trait TyArithParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends ArithParser[E, L] with BoolType.Lexer with NatType.Lexer {
  val pTyArithLNGE = pArithLNGE
  val pTyArithLNGT = new BoolType.Parser[T, L]() {}.pT | new NatType.Parser[T, L]() {}.pT
}

trait TyArithAlg[E, T] extends ArithAlg[E] with BoolType.Alg[T] with NatType.Alg[T]

trait TyArithPrint extends TyArithAlg[String, String] with ArithPrint with BoolType.Print with NatType.Print