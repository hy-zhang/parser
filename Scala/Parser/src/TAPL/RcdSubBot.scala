package TAPL

import Util._

/* <9> */
trait RcdSubBotParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends BotParser[E, T, L] with TypedRecord.Lexer {
  val pTypedRecordET = new TypedRecord.Parser[E, T, L]() {}
  val pRcdSubBotLNGE = pBotLNGE | pTypedRecordET.pE
  val pRcdSubBotLNGT = pBotLNGT | pTypedRecordET.pT
}

trait RcdSubBotAlg[E, T] extends BotAlg[E, T] with TypedRecord.Alg[E, T]

trait RcdSubBotPrint extends RcdSubBotAlg[String, String] with BotPrint with TypedRecord.Print

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
}