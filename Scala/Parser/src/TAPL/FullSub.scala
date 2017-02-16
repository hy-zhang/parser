package TAPL

import TAPL.Lib._


object FullSub {

  trait Alg[E, T] extends Simple.Alg[E, T] with Top.Alg[T]

  trait Print extends Alg[String, String] with Simple.Print with Top.Print

  trait Parse[E, T] extends Simple.Parse[E, T] with Top.Parse[T] {
    override val alg: Alg[E, T]

    val pFullSubE: Parser[E] = pSimpleE
    val pFullSubT: Parser[T] = pSimpleT ||| pTopT

    override val pE: Parser[E] = pFullSubE
    override val pT: Parser[T] = pFullSubT
  }

}

object TestFullSub {

  def parseWithAlg[E, T](inp: String)(a: FullSub.Alg[E, T]): E = {
    val p = new FullSub.Parse[E, T] {
      override val alg: FullSub.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullSub.Print {}))

}
