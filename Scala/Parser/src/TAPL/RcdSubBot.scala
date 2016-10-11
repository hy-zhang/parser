package TAPL

import Util._

/* <9> */
object RcdSubBot {

  trait Alg[T] {
    def TyRecord(l: List[(String, T)]): T
  }

  trait Print extends Alg[String] {
    def TyRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }

  trait Lexer {
    lexical.delimiters += ("{", "}", ",", ":")
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord
    }
  }

}
/*

trait RcdSubBotParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends FullRefParser[E, T, L] with RcdSubBot.Lexer {
  val pRcdSubBotT = new RcdSubBot.Parser[T, L]() {}
  val pRcdSubBotLNGE = pFullRefLNGE
  val pRcdSubBotLNGT = pFullRefLNGT | pRcdSubBotT.pT
}

// todo: extend from SimpleBool?
trait RcdSubBotAlg[E, T] extends FullRefAlg[E, T] with RcdSubBot.Alg[T]

trait RcdSubBotPrint extends RcdSubBotAlg[String, String] with FullRefPrint with RcdSubBot.Print

object TestRcdSubBot {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: RcdSubBotAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new RcdSubBotParser[E, T, List[E, T]] {}
      new List[E, T](lang.pRcdSubBotLNGE(alg)(l), lang.pRcdSubBotLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new RcdSubBotPrint {})

  def main(args: Array[String]) = {
    List(
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x"
    ).foreach(parseAndPrint)
  }
}*/