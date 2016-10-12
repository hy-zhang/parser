package TAPL

import Util._

object Top {

  trait Alg[T] {
    def TyTop(): T
  }

  trait Print extends Alg[String] {
    def TyTop() = "Top"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lexical.reserved += "Top"

    val pTopT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      "Top" ^^ { _ => alg.TyTop() }
    }
  }

}

object TopBot {

  trait Alg[T] extends Top.Alg[T] {
    def TyBot(): T
  }

  trait Print extends Alg[String] with Top.Print {
    def TyBot() = "Bot"
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] extends Top.Parser[T, F] {
    lexical.reserved += "Bot"

    val pBotT: Alg[T] => (=> F) => PackratParser[T] = alg => l => "Bot" ^^ { _ => alg.TyBot() }
    val pTopBotT: Alg[T] => (=> F) => PackratParser[T] = pTopT | pBotT
  }

}

trait BotParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends Typed.Parser[E, T, L] with TopBot.Parser[T, L] {
  val pBotLNGE = pTypedE
  val pBotLNGT = pTypedT | pTopBotT
}

trait BotAlg[E, T] extends Typed.Alg[E, T] with TopBot.Alg[T]

trait BotPrint extends BotAlg[String, String] with Typed.Print with TopBot.Print

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