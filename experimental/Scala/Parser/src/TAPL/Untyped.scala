package TAPL

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

trait Untyped extends StandardTokenParsers with PackratParsers {
  lexical.delimiters += ("\\", ".", "(", ")")
  
  trait List[E] {
    val pE : PackratParser[E]
  }
  
  trait UntypedAlg[E] {
    def id(x : String) : E
    def lam(x : String, e : E) : E
    def app(e1 : E, e2 : E) : E
  }
  
  trait Pretty extends UntypedAlg[String] {
    def id(x : String) = x
    def lam(x : String, e : String) = "\\" + x + "." + e
    def app(e1 : String, e2 : String) = e1 + " " + e2
  }
  
  trait Parser[E] {
    val pE : UntypedAlg[E] => (=> List[E]) => List[E] = alg => l => new List[E]() {
      lazy val e = l.pE
      override val pE : PackratParser[E] = ("\\" ~> ident) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |
        e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |
        "(" ~> e <~ ")" |
        ident ^^ alg.id
    }
  }
}

object TestUntyped extends Untyped {
  def parse(in : String) : Unit = {
    val p = new Parser[String](){}.pE(new Pretty(){})
    phrase(Util.fix(p).pE)(new lexical.Scanner(in)) match {
      case t if t.successful => println(t.get)
      case t                 => scala.sys.error(t.toString)
    }
  }
  def main(args : Array[String]) = {
    parse("\\x.x x")
  }
}