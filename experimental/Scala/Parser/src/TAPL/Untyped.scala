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
  
  trait Parser[E, F[E] <: {val pE : PackratParser[E]}] {
    lazy val pE : UntypedAlg[E] => (=> F[E]) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      ident ^^ alg.id |||
      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |||
      e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
      "(" ~> e <~ ")"
    }
  }
  
  def pE[F[String] <: {val pE : PackratParser[String]}] = {
    new Parser[String, F](){}.pE(new Pretty(){})
  }
}

object TestUntyped extends Arith.Lexer with Untyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  
  lazy val pArith = Arith.pE[List]
  lazy val pUntyped = Untyped.pE[List]
  lazy val p : (=> List[String]) => List[String] = l => new List[String]() {
    override lazy val pE = List(pArith, pUntyped).reduce(alt[String, List])(l)
  }
  lazy val parse = runParser(fix(p).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("if pred 1 then false else succ x")
    parse("if \\x.x x x then if true then false else 0 else false")
  }
}