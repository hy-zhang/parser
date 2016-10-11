package TAPL

import Util._

/* <4> */
object TyArith {

  trait Alg[T] {
    def nat(): T

    def bool(): T
  }

  trait Print extends Alg[String] {
    def nat() = "Nat"

    def bool() = "Bool"
  }

  trait Lexer {
    lexical.reserved += ("Nat", "Bool")
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Nat" ^^ { _ => alg.nat() } ||| "Bool" ^^ { _ => alg.bool() }
    }
  }

}

trait TyArithParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends ArithParser[E, L] with TyArith.Lexer {
  val pTyArithT = new TyArith.Parser[T, L] {}
  val pTyArithLNGE = pArithLNGE
  val pTyArithLNGT = pTyArithT.pT
}

trait TyArithAlg[E, T] extends ArithAlg[E] with TyArith.Alg[T]

trait TyArithPrint extends TyArithAlg[String, String] with ArithPrint with TyArith.Print