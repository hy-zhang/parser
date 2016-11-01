package TAPL

import TAPL.Util._

object EquiRec {

  trait Alg[E, T] extends Typed.Alg[E, T] with RecType.Alg[T] with TypeVar.Alg[T]

  trait Print extends Alg[String, String] with Typed.Print with RecType.Print with TypeVar.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Typed.Parser[E, T, L] with RecType.Parser[T, L] with TypeVar.Parser[T, L] {
    val pEquiRecE = pTypedE
    val pEquiRecT = pTypedT | pRecTypeT | pTypeVarT
  }

}

object TestEquiRec {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: EquiRec.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new EquiRec.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pEquiRecE(alg)(l), lang.pEquiRecT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new EquiRec.Print {})

}