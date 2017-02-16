package TAPL

import TAPL.Lib._


object EquiRec {

  trait Alg[E, T] extends Typed.Alg[E, T] with RecType.Alg[T] with TypeVar.Alg[T]

  trait Print extends Alg[String, String] with Typed.Print with RecType.Print with TypeVar.Print

  trait Parse[E, T] extends Typed.Parse[E, T] with RecType.Parse[T] with TypeVar.Parse[T] {
    override val alg: Alg[E, T]

    val pEquiRecE: Parser[E] = pTypedE
    val pEquiRecT: Parser[T] = pTypedT ||| pRecTypeT ||| pTypeVarT

    override val pE: Parser[E] = pEquiRecE
    override val pT: Parser[T] = pEquiRecT
  }

}

object TestEquiRec {

  def parseWithAlg[E, T](inp: String)(a: EquiRec.Alg[E, T]): E = {
    val p = new EquiRec.Parse[E, T] {
      override val alg: EquiRec.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new EquiRec.Print {}))

}