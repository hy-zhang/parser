package TAPL

import TAPL.Util._

object FullSub {

  trait Alg[E, T] extends Simple.Alg[E, T] with Top.Alg[T]

  trait Print extends Alg[String, String] with Simple.Print with Top.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Simple.Parser[E, T, L] with Top.Parser[T, L] {
    val pFullSubE = pSimpleE
    val pFullSubT = pSimpleT | pTopT
  }

}

object TestFullSub {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullSub.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullSub.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullSubE(alg)(l), lang.pFullSubT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullSub.Print {})

}
