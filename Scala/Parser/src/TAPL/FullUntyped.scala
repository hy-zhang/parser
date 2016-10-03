package TAPL

/* <3> */
object FullUntyped {
  import Util._
  trait Lexer extends FullUntypedAlg[String] {
    lexical.reserved += ("let", "in")
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=", "*")
    def record(l : List[(String, String)]) = "{" + l.map(x => x._1 + " = " + x._2).reduce((x, y) => x + ", " + y) + "}"
    def proj(e : String, x : String) = e + "." + x
    def float(d : Double) = d.toString()
    def timesfloat(e1 : String, e2 : String) = "timesfloat (" + e1 + ") (" + e2 + ")"
    def string(s : String) = "\"" + s + "\""
    def let(x : String, e1 : String, e2 : String) = "let " + x + " = " + e1 + " in " + e2
  }
  
  type ParserT[E] = {val pE : PackratParser[E]}
  trait FullUntypedAlg[E] {
    def record(l : List[(String, E)]) : E
    def proj(e : E, x : String) : E
    def float(d : Double) : E // Not supported for now due to the limit of StandardTokenParsers.
    // See http://jim-mcbeath.blogspot.hk/2008/09/scala-parser-combinators.html for solution.
    def timesfloat(e1 : E, e2 : E) : E // "timesfloat 0 0" causes ambiguity: app is applied to (0 0).
    // Therefore we need a delimiter.
    def string(s : String) : E
    def let(x : String, e1 : E, e2 : E) : E
  }
  
  trait Parser[E, F <: ParserT[E]] {
    lazy val pE : FullUntypedAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      "{" ~> repsep(lcid ~ ("=" ~> e) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.record |||
      e ~ ("." ~> lcid) ^^ { case e ~ x => alg.proj(e, x) } |||
      chainl1(e, "*" ^^^ {(e1 : E, e2 : E) => alg.timesfloat(e1, e2)}) |||
      ("let" ~> lcid) ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case x ~ e1 ~ e2 => alg.let(x, e1, e2) } |||
      stringLit ^^ alg.string |||
      "(" ~> e <~ ")"
    }
  }
    
  def pE[E, F <: ParserT[E]] = new Parser[E, F](){}
}

object TestFullUntyped extends Arith.Lexer with Untyped.Lexer with FullUntyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  trait Parse[E] {
    type L = List[E]
    val pArithE = Arith.pE[E, L]
    val pUntypedE = Untyped.pE[E, L]
    val pFullUntypedE = FullUntyped.pE[E, L]
    val pE = pArithE.pE | pUntypedE.pE | pFullUntypedE.pE
  }
  
  lazy val parser : (=> List[String]) => List[String] = l => new List[String]() {
    lazy val pE = new Parse[String](){}.pE(TestFullUntyped)(l)
  }
  lazy val parse = runParser(fix(parser).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("{x = 0, y = \\x. x x}.y")
    parse("\"\\x.x\"")
    parse("let x = false in \\y. y x")
    parse("(\\x.x) * succ 1")
    parse("1.2")
  }
}