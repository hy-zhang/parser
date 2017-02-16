package TAPL

import TAPL.Lib._


object FullRecon {

  trait Alg[E, T] extends Recon.Alg[E, T] with Let.Alg[E]

  trait Print extends Alg[String, String] with Recon.Print with Let.Print

  trait Parse[E, T] extends Recon.Parse[E, T] with Let.Parse[E] {
    override val alg: Alg[E, T]

    val pFullReconE: Parser[E] = pReconE ||| pLetE
    val pFullReconT: Parser[T] = pReconT

    override val pE: Parser[E] = pFullReconE
    override val pT: Parser[T] = pFullReconT
  }

}

object TestFullRecon {

  def parseWithAlg[E, T](inp: String)(a: FullRecon.Alg[E, T]): E = {
    val p = new FullRecon.Parse[E, T] {
      override val alg: FullRecon.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullRecon.Print {}))

}