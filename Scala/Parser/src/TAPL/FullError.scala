package TAPL

import Util._

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
      val e = l.pE

      "error" ^^ { _ => alg.TmError() } |||
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
    }
  }

}

trait FullErrorParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends BotParser[E, T, L] with TypedBool.Parser[E, T, L] with Error.Parser[E, L] with TypeVar.Parser[T, L] {
  val pFullErrorLNGE = pBotLNGE | pTypedBoolE | pErrorE
  val pFullErrorLNGT = pBotLNGT | pTypedBoolT | pTypeVarT
}

trait FullErrorAlg[E, T] extends BotAlg[E, T] with TypedBool.Alg[E, T] with Error.Alg[E] with TypeVar.Alg[T]

trait FullErrorPrint extends FullErrorAlg[String, String]
  with BotPrint with TypedBool.Print with Error.Print with TypeVar.Print

object TestFullError {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullErrorAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullErrorParser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullErrorLNGE(alg)(l), lang.pFullErrorLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullErrorPrint {})

  def main(args: Array[String]) = {
    List(
      "\\x:Top.if error then (try x with true) else false",
      "error true",
      "(\\x:Bool.x) error",
      "\\x:A.x"
    ).foreach(parseAndPrint)
  }
}