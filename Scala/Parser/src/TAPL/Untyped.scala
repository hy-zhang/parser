package TAPL

import Util._

/* <2> */
object Untyped {

  trait Alg[E] {
    def id(x: String): E

    def lam(x: String, e: E): E

    def app(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def id(x: String) = x

    def lam(x: String, e: String) = "\\" + x + "." + e

    def app(e1: String, e2: String) = "[" + e1 + " " + e2 + "]"
  }

  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        ident ^^ alg.id,
        ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) },
        e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait UntypedParser[E, L <: {val pE : Util.PackratParser[E]}] extends Untyped.Lexer {
  val pUntypedE = new Untyped.Parser[E, L]() {}
  val pUntypedLNGE = pUntypedE.pE
}

trait UntypedAlg[E] extends Untyped.Alg[E]

trait UntypedPrint extends UntypedAlg[String] with Untyped.Print

object TestUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parse[E](inp: String)(alg: UntypedAlg[E]) = {
    def parser(l: => List[E]): List[E] = {
      val lang = new UntypedParser[E, List[E]] {}
      new List[E](lang.pUntypedLNGE(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new UntypedPrint {})

  def main(args: Array[String]) = {
    List(
      "x",
      "\\x.x",
      "(\\x.x) \\x.x",
      "\\x.x (x \\x.x)"
    ).foreach(parseAndPrint)
  }
}