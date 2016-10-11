package TAPL

import Util._

/* <2> */
object UntypedAbs {

  trait Alg[E] {
    def TmAbs(x: String, e: E): E
  }

  trait Print extends Alg[String] {
    def TmAbs(x: String, e: String) = "\\" + x + "." + e
  }

  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.TmAbs(x, e0) } ||| "(" ~> e <~ ")"
    }
  }

}

object VarApp {

  trait Alg[E] {
    def TmVar(x: String): E

    def TmApp(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmVar(x: String) = x

    def TmApp(e1: String, e2: String) = "[" + e1 + " " + e2 + "]"
  }

  trait Lexer {
    lexical.delimiters += ("(", ")")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        ident ^^ alg.TmVar,
        e ~ e ^^ { case e1 ~ e2 => alg.TmApp(e1, e2) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait UntypedParser[E, L <: {val pE : Util.PackratParser[E]}]
  extends UntypedAbs.Lexer with VarApp.Lexer {
  val pUntypedLNGE = new UntypedAbs.Parser[E, L]() {}.pE | new VarApp.Parser[E, L]() {}.pE
}

trait UntypedAlg[E] extends UntypedAbs.Alg[E] with VarApp.Alg[E]

trait UntypedPrint extends UntypedAlg[String] with UntypedAbs.Print with VarApp.Print

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