package TAPL

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Untyped {
  import Util._
  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")")
  }
  
  trait UntypedAlg[E] {
    def id(x : String) : E
    def lam(x : String, e : E) : E
    def app(e1 : E, e2 : E) : E
  }
  
  trait Pretty extends UntypedAlg[String] {
    def id(x : String) = x
    def lam(x : String, e : String) = "\\" + x + "." + e
    def app(e1 : String, e2 : String) = "[" + e1 + " " + e2 + "]"
  }
  
  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    val pE : UntypedAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      ident ^^ alg.id |||
      ("\\" ~> ident) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |||
      e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
      "(" ~> e <~ ")"
    }
  }
}

object TestUntyped extends Arith.Lexer with Untyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  val pArith = new Arith.Parser[String, List[String]](){}.pE(new Arith.Pretty(){})
  val pUntyped = new Untyped.Parser[String, List[String]](){}.pE(new Untyped.Pretty(){})
  val pp : (=> List[String]) => PackratParser[String] = l => pArith(l) ||| pUntyped(l)
  val p : (=> List[String]) => List[String] = l => new List[String]() {
    override val pE = pp(l)
  }
  val parse = runParser(fix(p).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("if pred 1 then false else succ x")
    parse("if \\x.x x x then if true then false else 0 else false")
  }
}