package TAPL

import TAPL.Lib._


object UntypedAbs {

  trait Alg[E] {
    def TmAbs(x: String, e: E): E
  }

  trait Print extends Alg[String] {
    def TmAbs(x: String, e: String): String = "\\" + x + "." + e
  }

  trait Parse[E] extends EParser[E] {
    lexical.delimiters += ("\\", ".")

    val alg: Alg[E]

    val pUntypedAbsE: Parser[E] =
      ("\\" ~> lcid) ~ ("." ~> pE) ^^ { case x ~ e0 => alg.TmAbs(x, e0) }
  }

}

object VarApp {

  trait Alg[E] {
    def TmVar(x: String): E

    def TmApp(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmVar(x: String): String = x

    def TmApp(e1: String, e2: String): String = "[" + e1 + " " + e2 + "]"
  }

  trait Parse[E] extends EParser[E] {
    lexical.delimiters += ("(", ")")

    val alg: Alg[E]

    val pVarAppE: Parser[E] = {
      lcid ^^ alg.TmVar |||
        pE ~ pE ^^ { case e1 ~ e2 => alg.TmApp(e1, e2) } |||
        "(" ~> pE <~ ")"
    }
  }

}

object Untyped {

  trait Alg[E] extends UntypedAbs.Alg[E] with VarApp.Alg[E]

  trait Print extends Alg[String] with UntypedAbs.Print with VarApp.Print

  trait Parse[E] extends UntypedAbs.Parse[E] with VarApp.Parse[E] {
    override val alg: Alg[E]

    val pUntypedE: Parser[E] = pUntypedAbsE ||| pVarAppE

    override val pE: Parser[E] = pUntypedE
  }

}

object TestUntyped {

  def parseWithAlg[E](inp: String)(a: Untyped.Alg[E]): E = {
    val p = new Untyped.Parse[E] {
      override val alg: Untyped.Alg[E] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new Untyped.Print {}))
}