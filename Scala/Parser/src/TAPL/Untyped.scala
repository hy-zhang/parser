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

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.delimiters += ("\\", ".", "(", ")")

    val pUntypedAbsE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
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

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.delimiters += ("(", ")")

    val pVarAppE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        lcid ^^ alg.TmVar,
        e ~ e ^^ { case e1 ~ e2 => alg.TmApp(e1, e2) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Untyped {

  trait Alg[E] extends UntypedAbs.Alg[E] with VarApp.Alg[E]

  trait Print extends Alg[String] with UntypedAbs.Print with VarApp.Print

  trait Parser[E, L <: {val pE : Util.PackratParser[E]}]
    extends UntypedAbs.Parser[E, L] with VarApp.Parser[E, L] {
    val pUntypedE = pUntypedAbsE | pVarAppE
  }

}

object TestUntyped {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parse[E](inp: String)(alg: Untyped.Alg[E]) = {
    def parser(l: => List[E]): List[E] = {
      val lang = new Untyped.Parser[E, List[E]] {}
      new List[E](lang.pUntypedE(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new Untyped.Print {})

  def main(args: Array[String]) = {
    List(
      "x",
      "\\x.x",
      "(\\x.x) \\x.x",
      "\\x.x (x \\x.x)"
    ).foreach(parseAndPrint)
  }
}