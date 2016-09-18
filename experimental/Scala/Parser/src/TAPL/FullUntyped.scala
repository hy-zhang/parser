package TAPL

object FullUntyped {
  import Util._
  trait Lexer {
    lexical.reserved += ("let", "in")
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=", "*")
  }
  
  trait FullUntypedAlg[E] {
    def record(l : List[(String, E)]) : E
    def proj(e : E, x : String) : E
    def float(d : Double) : E // ???
    def timesfloat(e1 : E, e2 : E) : E // "timesfloat 0 0" causes ambiguity: app is applied to (0 0).
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
  
  trait Parser[E, F[E] <: {val pE : PackratParser[E]}] {
    lazy val pE : FullUntypedAlg[E] => (=> F[E]) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      "{" ~> repsep(lcid ~ ("=" ~> e) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.record |||
      e ~ ("." ~> lcid) ^^ { case e ~ x => alg.proj(e, x) } |||
      chainl1(e, "*" ^^^ {(e1 : E, e2 : E) => alg.timesfloat(e1, e2)}) |||
      ("let" ~> lcid) ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case x ~ e1 ~ e2 => alg.let(x, e1, e2) } |||
      stringLit ^^ alg.string |||
      "(" ~> e <~ ")"
    }
  }
  
  def pE[F[String] <: {val pE : PackratParser[String]}] = {
    new Parser[String, F](){}.pE(new Pretty(){})
  }
}

object TestFullUntyped extends Arith.Lexer with Untyped.Lexer with FullUntyped.Lexer {
  import Util._
  trait List[E] { val pE : PackratParser[E] }
  
  lazy val pArith = Arith.pE[List]
  lazy val pUntyped = Untyped.pE[List]
  lazy val pFullUntyped = FullUntyped.pE[List]
  lazy val p : (=> List[String]) => List[String] = l => new List[String]() {
    override lazy val pE = List(pArith, pUntyped, pFullUntyped).reduce(alt[String, List])(l)
  }
  lazy val parse = runParser(fix(p).pE)
 
  def main(args : Array[String]) = {
    parse("\\x.x (x \\x.x)")
    parse("{x = 0, y = \\x. x x}.y")
    parse("\"\\x.x\"")
    parse("let x = false in \\y. y x")
    parse("(\\x.x) * succ 1")
    parse("1.2")
  }
}