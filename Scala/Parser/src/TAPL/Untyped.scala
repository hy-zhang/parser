package TAPL

/* <2> */
object Untyped {
  import Util._
  trait Lexer extends UntypedAlg[String] {
    lexical.delimiters += ("\\", ".", "(", ")")
    def id(x : String) = x
    def lam(x : String, e : String) = "\\" + x + "." + e
    def app(e1 : String, e2 : String) = "[" + e1 + " " + e2 + "]"
  }
  
  type ParserT[E] = {val pE : PackratParser[E]}
  trait UntypedAlg[E] {
    def id(x : String) : E
    def lam(x : String, e : E) : E
    def app(e1 : E, e2 : E) : E
  }
  
  trait Parser[E, F <: ParserT[E]] {
    lazy val pE : UntypedAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      ident ^^ alg.id |||
      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |||
      e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
      "(" ~> e <~ ")"
    }
  }
  
  def pE[E, F <: ParserT[E]] = new Parser[E, F](){}
}

object TestUntyped extends Arith.Lexer with Untyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  trait Parse[E] {
    type L = List[E]
    val pArithE = Arith.pE[E, L]
    val pUntypedE = Untyped.pE[E, L]
    val pE = pArithE.pE | pUntypedE.pE
  }
  
  lazy val parser : (=> List[String]) => List[String] = l => new List[String]() {
    lazy val pE = new Parse[String](){}.pE(TestUntyped)(l)
  }
  lazy val parse = runParser(fix(parser).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("if pred 1 then false else succ x")
    parse("if \\x.x x x then if true then false else 0 else false")
  }
}