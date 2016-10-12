package TAPL

import Util._

trait EquiRecParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends Typed.Parser[E, T, L] with RecType.Parser[T, L] with TypeVar.Parser[T, L] {
  val pEquiRecLNGE = pTypedE
  val pEquiRecLNGT = pTypedT | pRecTypeT | pTypeVarT
}

trait EquiRecAlg[E, T] extends Typed.Alg[E, T] with RecType.Alg[T] with TypeVar.Alg[T]

trait EquiRecPrint extends EquiRecAlg[String, String]
  with Typed.Print with RecType.Print with TypeVar.Print

object TestEquiRec {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: EquiRecAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new EquiRecParser[E, T, List[E, T]] {}
      new List[E, T](lang.pEquiRecLNGE(alg)(l), lang.pEquiRecLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new EquiRecPrint {})

  def main(args: Array[String]) = {
    List(
      "\\x:A.x",
      "\\f:(Rec X.A->A).\\x:A.f x"
    ).foreach(parseAndPrint)
  }
}