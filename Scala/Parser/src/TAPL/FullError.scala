package TAPL


object FullError {

  import Util._

  trait Lexer {
    lexical.reserved += ("error", "try", "with")
  }

  type ParserSig[E] = {
    val pE: PackratParser[E]
  }

  trait FullErrorAlg[E] {
    def TmError(): E

    def TmTry(e1: E, e2: E): E
  }

  trait PrettyPrint extends FullErrorAlg[String] {
    def TmError() = "error"

    def TmTry(e1: String, e2: String) = "try " + e1 + " with " + e2
  }

  trait Parser[E, F <: ParserSig[E]] {
    lazy val pE: FullErrorAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        "error" ^^ { _ => alg.TmError() },
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
      ).reduce((a, b) => a ||| b)
    }
  }

  def pE[F <: ParserSig[String]] = {
    new Parser[String, F]() {}.pE(new PrettyPrint() {})
  }
}

object TestFullError extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer with FullError.Lexer {

  import Util._

  object parser {

    trait ParserSig[E, T] {
      val pE: PackratParser[E]
      val pT: PackratParser[T]
    }

    type R = ParserSig[String, String]

    lazy val pSimpleBoolET = SimpleBool.pET[R]
    lazy val pFullSimpleET = FullSimple.pET[R]

    val p: (=> R) => R = l => new R() {
      override lazy val pE = List(
        Arith.pE[R], FullUntyped.pE[R], pSimpleBoolET._1, pFullSimpleET._1, FullRef.pE[R], FullError.pE[R]
      ).reduce(alt[String, R])(l)

      override lazy val pT = List(
        TyArith.pT[R], pSimpleBoolET._2, pFullSimpleET._2, FullRef.pT[R]
      ).reduce(alt[String, R])(l)
    }
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    List(
      "\\x:Top.if error then (try x with true) else false",
      "error true",
      "(\\x:Bool.x) error"
    ).foreach(parse)
  }
}