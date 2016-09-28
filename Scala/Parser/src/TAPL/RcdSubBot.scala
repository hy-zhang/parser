package TAPL


object RcdSubBot {

  import Util._

  trait Lexer {
    lexical.delimiters += ("{", "}", ",", ":")
  }

  type ParserSig[T] = {
    val pT: PackratParser[T]
  }

  trait RcdSubBotAlg[T] {
    def TyRecord(l: List[(String, T)]): T
  }

  trait PrettyPrint extends RcdSubBotAlg[String] {
    def TyRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }

  trait Parser[T, F <: ParserSig[T]] {
    lazy val pT: RcdSubBotAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord
    }
  }

  def pT[F <: ParserSig[String]] = {
    new Parser[String, F]() {}.pT(new PrettyPrint() {})
  }
}

object TestRcdSubBot extends FullUntyped.Lexer with SimpleBool.Lexer with FullRef.Lexer {

  import Util._

  object parser {

    trait ParserSig[E, T] {
      val pE: PackratParser[E]
      val pT: PackratParser[T]
    }

    type R = ParserSig[String, String]

    val p: (=> R) => R = l => new R() {
      override lazy val pE = List(FullUntyped.pE[R], SimpleBool.pET[R]._1, FullRef.pE[R]).reduce(alt[String, R])(l)
      override lazy val pT = List(SimpleBool.pET[R]._2, FullRef.pT[R], RcdSubBot.pT[R]).reduce(alt[String, R])(l)
    }
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    List(
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x"
    ).foreach(parse)
  }
}
