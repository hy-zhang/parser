package TAPL

import TAPL.Util._

/* <8> */
object Error {

  trait Alg[E] {
    def TmError(): E

    def TmTry(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmError() = "error"

    def TmTry(e1: String, e2: String) = "try " + e1 + " with " + e2
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.reserved += ("error", "try", "with")

    val pErrorE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      "error" ^^ { _ => alg.TmError() } |||
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
    }
  }

}

object FullError {

  trait Alg[E, T] extends Bot.Alg[E, T] with TypedBool.Alg[E, T] with Error.Alg[E] with TypeVar.Alg[T]

  trait Print extends Alg[String, String]
    with Bot.Print with TypedBool.Print with Error.Print with TypeVar.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Bot.Parser[E, T, L] with TypedBool.Parser[E, T, L] with Error.Parser[E, L] with TypeVar.Parser[T, L] {
    val pFullErrorE = pBotE | pTypedBoolE | pErrorE
    val pFullErrorT = pBotT | pTypedBoolT | pTypeVarT
  }

}

object TestFullError {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullError.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullError.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullErrorE(alg)(l), lang.pFullErrorT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullError.Print {})

}