package TAPL


object FullIsoRec {

  import Util._

  trait Lexer {
    lexical.reserved += ("fold", "unfold")
    lexical.delimiters += ("[", "]")
  }

  type ParserSig[E, T] = {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
  }

  trait FullIsoAlg[E, T] {
    def TmFold(e: E, t: T): E

    def TmUnfold(e: E, t: T): E
  }

  trait PrettyPrint extends FullIsoAlg[String, String] {
    def TmFold(e: String, t: String) = "fold [" + t + "] " + e

    def TmUnfold(e: String, t: String) = "unfold [" + t + "] " + e
  }

  trait Parser[E, T, F <: ParserSig[E, T]] {
    lazy val pE: FullIsoAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        "fold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmFold(ex, ty) },
        "unfold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmUnfold(ex, ty) }
      ).reduce((a, b) => a ||| b)
    }
  }

  def pE[F <: ParserSig[String, String]] = {
    new Parser[String, String, F]() {}.pE(new PrettyPrint() {})
  }
}

object TestFullIsoRec extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer with FullError.Lexer with RcdSubBot.Lexer with FullIsoRec.Lexer {

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
        Arith.pE[R], FullUntyped.pE[R], pSimpleBoolET._1, pFullSimpleET._1, FullRef.pE[R], FullError.pE[R], FullIsoRec.pE[R]
      ).reduce(alt[String, R])(l)

      override lazy val pT = List(
        TyArith.pT[R], pSimpleBoolET._2, pFullSimpleET._2, FullRef.pT[R], RcdSubBot.pT[R]
      ).reduce(alt[String, R])(l)
    }
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    List(
      "fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get"
    ).foreach(parse)
  }
}