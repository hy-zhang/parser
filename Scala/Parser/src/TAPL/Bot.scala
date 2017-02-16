package TAPL

import TAPL.Lib._


object Top {

  trait Alg[T] {
    def TyTop(): T
  }

  trait Print extends Alg[String] {
    def TyTop() = "Top"
  }

  trait Parse[T] {
    lexical.reserved += "Top"

    val alg: Alg[T]

    val pTopT: Parser[T] = "Top" ^^ { _ => alg.TyTop() }
  }

}

object TopBot {

  trait Alg[T] extends Top.Alg[T] {
    def TyBot(): T
  }

  trait Print extends Alg[String] with Top.Print {
    def TyBot() = "Bot"
  }

  trait Parse[T] extends Top.Parse[T] {
    lexical.reserved += "Bot"

    override val alg: Alg[T]

    val pTopBotT: Parser[T] = pTopT ||| "Bot" ^^ { _ => alg.TyBot() }
  }

}

object Bot {

  trait Alg[E, T] extends Typed.Alg[E, T] with TopBot.Alg[T]

  trait Print extends Alg[String, String] with Typed.Print with TopBot.Print

  trait Parse[E, T] extends Typed.Parse[E, T] with TopBot.Parse[T] {
    override val alg: Alg[E, T]

    val pBotE: Parser[E] = pTypedE
    val pBotT: Parser[T] = pTypedT ||| pTopBotT

    override val pE: Parser[E] = pBotE

    override val pT: Parser[T] = pBotT
  }

}

object TestBot {

  def parseWithAlg[E, T](inp: String)(a: Bot.Alg[E, T]): E = {
    val p = new Bot.Parse[E, T] {
      override val alg: Bot.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new Bot.Print {}))

}