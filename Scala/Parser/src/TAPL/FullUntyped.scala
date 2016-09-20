package TAPL

/* <3> */
object FullUntyped {
  import Util._
  trait Lexer {
    lexical.reserved += ("let", "in")
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=", "*")
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
  
  trait Pretty extends FullUntypedAlg[String] {
    def record(l : List[(String, String)]) = "{" + l.map(x => x._1 + " = " + x._2).reduce((x, y) => x + ", " + y) + "}"
    def proj(e : String, x : String) = e + "." + x
    def float(d : Double) = d.toString()
    def timesfloat(e1 : String, e2 : String) = "timesfloat (" + e1 + ") (" + e2 + ")"
    def string(s : String) = "\"" + s + "\""
    def let(x : String, e1 : String, e2 : String) = "let " + x + " = " + e1 + " in " + e2
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
  
  def pE[F <: ParserT[String]] = {
    new Parser[String, F](){}.pE(new Pretty(){})
  }
}

object TestFullUntyped extends Arith.Lexer with Untyped.Lexer with FullUntyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  type L = List[String]
  
  trait Parse[E] {
    type L = List[E]
    type PE = (=> L) => PackratParser[E]
    val pArithE : PE
    val pUntypedE : PE
    val pFullUntypedE : PE
    val p : (=> L) => L = l => new L() {
      override lazy val pE = List(pArithE, pUntypedE, pFullUntypedE).reduce(alt[E, L])(l)
    }
  }
  
  lazy val parser = new Parse[String]() {
    lazy val pArithE = Arith.pE[L]
    lazy val pUntypedE = Untyped.pE[L]
    lazy val pFullUntypedE = FullUntyped.pE[L]
  }
  lazy val parse = runParser(fix(parser.p).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("{x = 0, y = \\x. x x}.y")
    parse("\"\\x.x\"")
    parse("let x = false in \\y. y x")
    parse("(\\x.x) * succ 1")
    parse("1.2")
  }
}