package TAPL

/* <2> */
object Untyped {
  import Util._
  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")")
  }
  
  type ParserT[E] = {val pE : PackratParser[E]}

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
  
  trait Parser[E, F <: ParserT[E]] {
    lazy val pE : UntypedAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      ident ^^ alg.id |||
      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |||
      e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
      "(" ~> e <~ ")"
    }
  }
  
  def pE[F <: ParserT[String]] = {
    new Parser[String, F](){}.pE(new Pretty(){})
  }
}

object TestUntyped extends Arith.Lexer with Untyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  type L = List[String]
  
  trait Parse[E] {
    type L = List[E]
    type PE = (=> L) => PackratParser[E]
    val pArithE : PE
    val pUntypedE : PE
    val p : (=> L) => L = l => new L() {
      override lazy val pE = List(pArithE, pUntypedE).reduce(alt[E, L])(l)
    }
  }
  
  lazy val parser = new Parse[String]() {
    lazy val pArithE = Arith.pE[L]
    lazy val pUntypedE = Untyped.pE[L]
  }
  lazy val parse = runParser(fix(parser.p).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("if pred 1 then false else succ x")
    parse("if \\x.x x x then if true then false else 0 else false")
  }
}