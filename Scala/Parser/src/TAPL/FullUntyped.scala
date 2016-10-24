package TAPL

import Util._

/* <3> */
object Record {

  trait Alg[E] {
    def TmRecord(l: List[(String, E)]): E

    def TmProj(e: E, x: String): E
  }

  trait Print extends Alg[String] {
    def TmRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + " = " + x._2).reduce((x, y) => x + ", " + y) + "}"

    def TmProj(e: String, x: String) = e + "." + x
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

    val pRecordE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        "{" ~> repsep(lcid ~ ("=" ~> e) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TmRecord,
        e ~ ("." ~> lcid) ^^ { case e ~ x => alg.TmProj(e, x) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Let {

  trait Alg[E] {
    def TmLet(x: String, e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmLet(x: String, e1: String, e2: String) = "let " + x + " = " + e1 + " in " + e2
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.reserved += ("let", "in")
    lexical.delimiters += "="

    val pLetE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      ("let" ~> lcid) ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case x ~ e1 ~ e2 => alg.TmLet(x, e1, e2) }
    }
  }

}

object FloatString {

  trait Alg[E] {
    def TmFloat(d: Double): E

    // Not supported for now due to the limit of StandardTokenParsers.
    // See http://jim-mcbeath.blogspot.hk/2008/09/scala-parser-combinators.html for solution.
    def TmTimesfloat(e1: E, e2: E): E

    // "timesfloat 0 0" causes ambiguity: app is applied to (0 0).
    // Therefore we need a delimiter.
    def TmString(s: String): E
  }

  trait Print extends Alg[String] {
    def TmFloat(d: Double) = d.toString

    def TmTimesfloat(e1: String, e2: String) = "timesfloat (" + e1 + ") (" + e2 + ")"

    def TmString(s: String) = "\"" + s + "\""
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.delimiters += "*"

    val pFloatStringE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      chainl1(e, "*" ^^^ { (e1: E, e2: E) => alg.TmTimesfloat(e1, e2) }) |||
        stringLit ^^ alg.TmString
    }
  }

}

object FullUntyped {

  trait Alg[E] extends Arith.Alg[E] with Untyped.Alg[E] with Record.Alg[E]
    with FloatString.Alg[E] with Let.Alg[E]

  trait Print extends Alg[String] with Arith.Print with Untyped.Print with Record.Print
    with FloatString.Print with Let.Print

  trait Parser[E, L <: {val pE : Util.PackratParser[E]}]
    extends Arith.Parser[E, L] with Untyped.Parser[E, L] with Record.Parser[E, L]
      with FloatString.Parser[E, L] with Let.Parser[E, L] {
    val pFullUntypedE = pArithE | pUntypedE | pRecordE | pFloatStringE | pLetE
  }

}

object TestFullUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parse[E](inp: String)(alg: FullUntyped.Alg[E]) = {
    def parser(l: => List[E]): List[E] = {
      val lang = new FullUntyped.Parser[E, List[E]] {}
      new List[E](lang.pFullUntypedE(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullUntyped.Print {})

  def main(args: Array[String]) = {
    List(
      "\\x.x (x \\x.x)",
      "{x = 0, y = \\x. x x}.y",
      "\"\\x.x\"",
      "let x = false in \\y. y x",
      "(\\x.x) * succ 1",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "1.2"
    ).foreach(parseAndPrint)
  }
}