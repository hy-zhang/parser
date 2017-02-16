package TAPL

import TAPL.Util._

object FullRecon {

  trait Alg[E, T] extends Recon.Alg[E, T] with Let.Alg[E]

  trait Print extends Alg[String, String] with Recon.Print with Let.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Recon.Parser[E, T, L] with Let.Parser[E, L] {
    val pFullReconE = pReconE | pLetE
    val pFullReconT = pReconT
  }

}

object TestFullRecon {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullRecon.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullRecon.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullReconE(alg)(l), lang.pFullReconT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullRecon.Print {})

}