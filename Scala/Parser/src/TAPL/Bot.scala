package TAPL

import Util._

object Top {

  trait Alg[T] {
    def TyTop(): T
  }

  trait Print extends Alg[String] {
    def TyTop() = "Top"
  }

  trait Lexer {
    lexical.reserved += "Top"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Top" ^^ { _ => alg.TyTop() }
    }
  }

}

object TopBot {

  trait BotAlg[T] {
    def TyBot(): T
  }

  trait Alg[T] extends Top.Alg[T] with BotAlg[T]

  trait Print extends Alg[String] with Top.Print {
    def TyBot() = "Bot"
  }

  trait Lexer extends Top.Lexer {
    lexical.reserved += "Bot"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lazy val topParser = new Top.Parser[T, F]() {}.pT
    lazy val botParser: BotAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Bot" ^^ { _ => alg.TyBot() }
    }
    lazy val pT: Alg[T] => (=> F) => PackratParser[T] = topParser | botParser
  }

}

trait BotParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TypedParser[E, T, L] with TopBot.Lexer {
  val pBotLNGE = pTypedLNGE
  val pBotLNGT = pTypedLNGT | new TopBot.Parser[T, L]() {}.pT
}

trait BotAlg[E, T] extends TypedAlg[E, T] with TopBot.Alg[T]

trait BotPrint extends BotAlg[String, String] with TypedPrint with TopBot.Print

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