package TAPL

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.collection.immutable.HashSet

trait Arith extends StandardTokenParsers with PackratParsers {
  lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters += ("(", ")")
  
  trait List[E] {
    val pE : PackratParser[E]
  }
  
  trait ArithAlg[E] {
    def trueV() : E
    def falseV() : E
    def ifS(e1 : E, e2 : E, e3 : E) : E
    def zero() : E
    def succ(e : E) : E
    def pred(e : E) : E
    def isZero(e : E) : E
  }
  
  trait Pretty extends ArithAlg[String] {
    def trueV() = "true"
    def falseV() = "false"
    def ifS(e1 : String, e2 : String, e3 : String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"
    def zero() = "0"
    def succ(e : String) = "succ (" + e + ")"
    def pred(e : String) = "pred (" + e + ")"
    def isZero(e : String) = "iszero (" + e + ")"
  }
  
  trait Parser[E] {
    val pE : ArithAlg[E] => (=> List[E]) => List[E] = alg => l => new List[E]() {
      lazy val e = l.pE
      def num(x : Int) : E = x match {
        case 0 => alg.zero()
        case _ => alg.succ(num(x - 1))
      }
      override val pE : PackratParser[E] = "true" ^^ { _ => alg.trueV() } |
        "false" ^^ { _ => alg.falseV() } |
        ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => alg.ifS(e1, e2, e3) } |
        numericLit ^^ { x => num(x.toInt) } |
        "succ" ~> e ^^ alg.succ |
        "pred" ~> e ^^ alg.pred |
        "iszero" ~> e ^^ alg.isZero |
        "(" ~> e <~ ")"
    }
   }
}

object TestArith extends Arith {
  def parse(in : String) : Unit = {
    val p = new Parser[String](){}.pE(new Pretty(){})
    phrase(Util.fix(p).pE)(new lexical.Scanner(in)) match {
      case t if t.successful => println(t.get)
      case t                 => scala.sys.error(t.toString)
    }
  }
  def main(args : Array[String]) = {
    parse("true")
    parse("if false then true else false")
    parse("3")
    parse("succ (pred 0)")
    parse("iszero (pred (succ (succ 0)))")
  }
}