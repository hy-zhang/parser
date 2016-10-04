package TAPL

/* <4> */
object TyArith {
  import Util._
  trait TyArithAlg[T] {
    def nat() : T
    def bool() : T
  }
  trait Lexer extends TyArithAlg[String] {
    lexical.reserved += ("Nat", "Bool")
    def nat() = "Nat"
    def bool() = "Bool"
  }
  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT : TyArithAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Nat" ^^ { _ => alg.nat() } |||
      "Bool" ^^ { _ => alg.bool() }
    }
  }
}