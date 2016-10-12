package TAPL

import Util._

/* <9> */
object RcdSubBot {

  trait Alg[E, T] extends Bot.Alg[E, T] with TypedRecord.Alg[E, T]

  trait Print extends Alg[String, String] with Bot.Print with TypedRecord.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Bot.Parser[E, T, L] with TypedRecord.Parser[E, T, L] {
    val pRcdSubBotE = pBotE | pTypedRecordE
    val pRcdSubBotT = pBotT | pTypedRecordT
  }

}

object TestRcdSubBot {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: RcdSubBot.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new RcdSubBot.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pRcdSubBotE(alg)(l), lang.pRcdSubBotT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new RcdSubBot.Print {})

  def main(args: Array[String]) = {
    List(
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x"
    ).foreach(parseAndPrint)
  }
}