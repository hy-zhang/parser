package TAPL

import Util._

/* <3> */
object FullUntyped {

  trait Alg[E] {
    def record(l: List[(String, E)]): E

    def proj(e: E, x: String): E

    def float(d: Double): E

    // Not supported for now due to the limit of StandardTokenParsers.
    // See http://jim-mcbeath.blogspot.hk/2008/09/scala-parser-combinators.html for solution.
    def timesfloat(e1: E, e2: E): E

    // "timesfloat 0 0" causes ambiguity: app is applied to (0 0).
    // Therefore we need a delimiter.
    def string(s: String): E

    def let(x: String, e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def record(l: List[(String, String)]) = "{" + l.map(x => x._1 + " = " + x._2).reduce((x, y) => x + ", " + y) + "}"

    def proj(e: String, x: String) = e + "." + x

    def float(d: Double) = d.toString

    def timesfloat(e1: String, e2: String) = "timesfloat (" + e1 + ") (" + e2 + ")"

    def string(s: String) = "\"" + s + "\""

    def let(x: String, e1: String, e2: String) = "let " + x + " = " + e1 + " in " + e2
  }

  trait Lexer {
    lexical.reserved += ("let", "in")
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=", "*")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        "{" ~> repsep(lcid ~ ("=" ~> e) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.record,
        e ~ ("." ~> lcid) ^^ { case e ~ x => alg.proj(e, x) },
        chainl1(e, "*" ^^^ { (e1: E, e2: E) => alg.timesfloat(e1, e2) }),
        ("let" ~> lcid) ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case x ~ e1 ~ e2 => alg.let(x, e1, e2) },
        stringLit ^^ alg.string,
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait FullUntypedParser[E, L <: {val pE : Util.PackratParser[E]}] extends UntypedParser[E, L] with FullUntyped.Lexer {
  val pFullUntypedE = new FullUntyped.Parser[E, L]() {}
  val pFullUntypedLNGE = pUntypedLNGE | pFullUntypedE.pE
}

trait FullUntypedAlg[E] extends UntypedAlg[E] with FullUntyped.Alg[E]

trait FullUntypedPrint extends FullUntypedAlg[String] with UntypedPrint with FullUntyped.Print

object TestFullUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parse[E](inp: String)(alg: FullUntypedAlg[E]) = {
    def parser(l: => List[E]): List[E] = {
      val lang = new FullUntypedParser[E, List[E]] {}
      new List[E](lang.pFullUntypedLNGE(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullUntypedPrint {})

  def main(args: Array[String]) = {
    List(
      "\\x.x (x \\x.x)",
      "{x = 0, y = \\x. x x}.y",
      "\"\\x.x\"",
      "let x = false in \\y. y x",
      "(\\x.x) * succ 1",
      "1.2"
    ).foreach(parseAndPrint)
  }
}