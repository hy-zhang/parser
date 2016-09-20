package TAPL

/* <4> */
object TyArith {
  import Util._
  trait Lexer {
    lexical.reserved += ("Nat", "Bool")
  }
  
  type ParserT[T] = {val pT : PackratParser[T]}
  
  trait TyArithAlg[T] {
    def nat() : T
    def bool() : T
  }
  
  trait Pretty extends TyArithAlg[String] {
    def nat() = "Nat"
    def bool() = "Bool"
  }
  
  trait Parser[T, F <: ParserT[T]] {
    lazy val pT : TyArithAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Nat" ^^ { _ => alg.nat() } |||
      "Bool" ^^ { _ => alg.bool() }
    }
  }
  
  def pT[F <: ParserT[String]] = {
    new Parser[String, F](){}.pT(new Pretty(){})
  }
}