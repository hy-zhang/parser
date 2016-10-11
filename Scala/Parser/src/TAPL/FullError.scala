package TAPL

import Util._

/* <8> */
object FullError {

  trait Alg[E] {
    def TmError(): E

    def TmTry(e1: E, e2: E): E
  }

  trait Print extends Alg[String] {
    def TmError() = "error"

    def TmTry(e1: String, e2: String) = "try " + e1 + " with " + e2
  }

  trait Lexer {
    lexical.reserved += ("error", "try", "with")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        "error" ^^ { _ => alg.TmError() },
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait FullErrorParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends SimpleBoolParser[E, T, L] with Bot.Lexer with FullError.Lexer {
  val pBotT = new Bot.Parser[T, L]() {}
  val pFullErrorE = new FullError.Parser[E, L]() {}
  val pFullErrorLNGE = pSimpleBoolLNGE | pFullErrorE.pE
  val pFullErrorLNGT = pSimpleBoolLNGT | pBotT.pT
}

trait FullErrorAlg[E, T] extends SimpleBoolAlg[E, T] with Bot.Alg[T] with FullError.Alg[E]

trait FullErrorPrint extends FullErrorAlg[String, String] with SimpleBoolPrint with Bot.Print with FullError.Print

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
      "(\\x:Bool.x) error"
    ).foreach(parseAndPrint)
  }
}