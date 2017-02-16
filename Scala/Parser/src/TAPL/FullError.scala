package TAPL

import TAPL.Lib._


object Error {

  trait Alg[E] {
    def TmError(): E

    def TmTry(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmError() = "error"

    def TmTry(e1: String, e2: String): String = "try " + e1 + " with " + e2
  }

  trait Parse[E] extends EParser[E] {
    lexical.reserved += ("error", "try", "with")

    val alg: Alg[E]

    val pErrorE: Parser[E] =
      "error" ^^ { _ => alg.TmError() } |||
        "try" ~> pE ~ ("with" ~> pE) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
  }

}

object FullError {

  trait Alg[E, T] extends Bot.Alg[E, T] with TypedBool.Alg[E, T] with Error.Alg[E] with TypeVar.Alg[T]

  trait Print extends Alg[String, String]
    with Bot.Print with TypedBool.Print with Error.Print with TypeVar.Print

  trait Parse[E, T] extends Bot.Parse[E, T] with TypedBool.Parse[E, T]
    with Error.Parse[E] with TypeVar.Parse[T] {

    override val alg: Alg[E, T]

    val pFullErrorE: Parser[E] = pBotE ||| pTypedBoolE ||| pErrorE
    val pFullErrorT: Parser[T] = pBotT ||| pTypedBoolT ||| pTypeVarT

    override val pE: Parser[E] = pFullErrorE
    override val pT: Parser[T] = pFullErrorT
  }

}

object TestFullError {

  def parseWithAlg[E, T](inp: String)(a: FullError.Alg[E, T]): E = {
    val p = new FullError.Parse[E, T] {
      override val alg: FullError.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullError.Print {}))

}