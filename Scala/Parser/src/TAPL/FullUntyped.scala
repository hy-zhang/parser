package TAPL

import TAPL.Lib._


object Record {

  trait Alg[E] {
    def TmRecord(l: List[(String, E)]): E

    def TmProj(e: E, x: String): E
  }

  trait Print extends Alg[String] {
    def TmRecord(l: List[(String, String)]): String =
      "{" + l.map(x => x._1 + " = " + x._2).reduce((x, y) => x + ", " + y) + "}"

    def TmProj(e: String, x: String): String = e + "." + x
  }

  trait Parse[E] extends EParser[E] {
    lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

    val alg: Alg[E]

    val pRecordE: Parser[E] =
      "{" ~> repsep(lcid ~ ("=" ~> pE) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TmRecord |||
        pE ~ ("." ~> lcid) ^^ { case e ~ x => alg.TmProj(e, x) }
  }

}

object Let {

  trait Alg[E] {
    def TmLet(x: String, e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmLet(x: String, e1: String, e2: String): String = "let " + x + " = " + e1 + " in " + e2
  }

  trait Parse[E] extends EParser[E] {
    lexical.reserved += ("let", "in")
    lexical.delimiters += "="

    val alg: Alg[E]

    val pLetE: Parser[E] =
      ("let" ~> lcid) ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ { case x ~ e1 ~ e2 => alg.TmLet(x, e1, e2) }
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
    def TmFloat(d: Double): String = d.toString

    def TmTimesfloat(e1: String, e2: String): String = "timesfloat (" + e1 + ") (" + e2 + ")"

    def TmString(s: String): String = "\"" + s + "\""
  }

  trait Parse[E] extends EParser[E] {
    lexical.delimiters += "*"

    val alg: Alg[E]

    val pFloatStringE: Parser[E] =
      chainl1(pE, "*" ^^^ { (e1: E, e2: E) => alg.TmTimesfloat(e1, e2) }) |||
        stringLit ^^ alg.TmString
  }

}

object FullUntyped {

  trait Alg[E] extends Arith.Alg[E] with Untyped.Alg[E] with Record.Alg[E]
    with FloatString.Alg[E] with Let.Alg[E]

  trait Print extends Alg[String] with Arith.Print with Untyped.Print with Record.Print
    with FloatString.Print with Let.Print

  trait Parse[E] extends Arith.Parse[E] with Untyped.Parse[E] with Record.Parse[E]
    with FloatString.Parse[E] with Let.Parse[E] {

    override val alg: Alg[E]

    val pFullUntypedE: Parser[E] = pArithE ||| pUntypedE ||| pRecordE ||| pFloatStringE ||| pLetE

    override val pE: Parser[E] = pFullUntypedE
  }

}

object TestFullUntyped {

  def parseWithAlg[E](inp: String)(a: FullUntyped.Alg[E]): E = {
    val p = new FullUntyped.Parse[E] {
      override val alg: FullUntyped.Alg[E] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullUntyped.Print {}))
}