package TAPL

import TAPL.Lib._


object RcdSubBot {

  trait Alg[E, T] extends Bot.Alg[E, T] with TypedRecord.Alg[E, T]

  trait Print extends Alg[String, String] with Bot.Print with TypedRecord.Print

  trait Parse[E, T] extends Bot.Parse[E, T] with TypedRecord.Parse[E, T] {
    override val alg: Alg[E, T]

    val pRcdSubBotE: Parser[E] = pBotE ||| pTypedRecordE
    val pRcdSubBotT: Parser[T] = pBotT ||| pTypedRecordT

    override val pE: Parser[E] = pRcdSubBotE
    override val pT: Parser[T] = pRcdSubBotT
  }

}

object TestRcdSubBot {

  def parseWithAlg[E, T](inp: String)(a: RcdSubBot.Alg[E, T]): E = {
    val p = new RcdSubBot.Parse[E, T] {
      override val alg: RcdSubBot.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new RcdSubBot.Print {}))

}