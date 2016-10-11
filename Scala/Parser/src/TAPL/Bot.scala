package TAPL

import Util._

object Bot {

  trait Alg[T] {
    def TyTop(): T

    def TyBot(): T
  }

  trait Print extends Alg[String] {
    def TyTop() = "Top"

    def TyBot() = "Bot"
  }

  trait Lexer {
    lexical.reserved += ("Top", "Bot")
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "Top" ^^ { _ => alg.TyTop() } ||| "Bot" ^^ { _ => alg.TyBot() }
    }
  }

}

trait BotParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends SimpleBool.Lexer with Bot.Lexer {
  val pSimpleBoolET = new SimpleBool.Parser[E, T, L]() {}
  val pBotT = new Bot.Parser[T, L]() {}
  val pBotLNGE = pSimpleBoolET.pE
  val pBotLNGT = pSimpleBoolET.pT | pBotT.pT
}

trait BotAlg[E, T] extends SimpleBool.Alg[E, T] with Bot.Alg[T]

trait BotPrint extends BotAlg[String, String] with SimpleBool.Print with Bot.Print

object TestBot {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: BotAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new BotParser[E, T, List[E, T]] {}
      new List[E, T](lang.pBotLNGE(alg)(l), lang.pBotLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new BotPrint {})

  def main(args: Array[String]) = {
    List(
      "\\x:Top.x",
      "(\\x:Top.x) \\x:Top.x",
      "(\\x:Top->Top.x) (\\x:Top.x)",
      "\\x:Bot.x",
      "\\x:Bot.x x"
    ).foreach(parseAndPrint)
  }
}