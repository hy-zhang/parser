package TAPL

import scala.collection.immutable.HashSet

/* <1> */
object Arith {
  import Util._ 
  trait ArithAlg[E] {
    def trueV() : E
    def falseV() : E
    def ifS(e1 : E, e2 : E, e3 : E) : E
    def zero() : E
    def succ(e : E) : E
    def pred(e : E) : E
    def isZero(e : E) : E
  } 
  trait Lexer extends ArithAlg[String] {
    lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")
    def trueV() = "true"
    def falseV() = "false"
    def ifS(e1 : String, e2 : String, e3 : String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"
    def zero() = "0"
    def succ(e : String) = "succ (" + e + ")"
    def pred(e : String) = "pred (" + e + ")"
    def isZero(e : String) = "iszero (" + e + ")"
  }
  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE : ArithAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      def num(x : Int) : E = x match {
        case 0 => alg.zero()
        case _ => alg.succ(num(x - 1))
      }
      "true" ^^ { _ => alg.trueV() } |||
      "false" ^^ { _ => alg.falseV() } |||
      ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => alg.ifS(e1, e2, e3) } |||
      numericLit ^^ { x => num(x.toInt) } |||
      "succ" ~> e ^^ alg.succ |||
      "pred" ~> e ^^ alg.pred |||
      "iszero" ~> e ^^ alg.isZero |||
      "(" ~> e <~ ")"
    }
  }
}

trait ArithLNG[E, L <: {val pE : Util.PackratParser[E]}] extends Arith.Lexer {
  val pArithE = new Arith.Parser[E, L](){}
  val pArithLNGE = pArithE.pE
  // we cannot use pE, such a name has incompatible types when overridden. is there a solution?
}

object TestArithLNG {
  import Util._
  class List[E](pe : PackratParser[E]) { val pE = pe }
  object Test extends ArithLNG[String, List[String]] {
    lazy val parser : (=> List[String]) => List[String] = l =>
      new List[String](pArithLNGE(Test)(l))
    lazy val parse = runParser(fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "true",
      "if false then true else false",
      "3",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))"
    ).foreach(Test.parse)
  }
}