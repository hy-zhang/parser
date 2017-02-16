package TAPL2.FullRecon

import TAPL2.Recon.Recon
import TAPL2.FullUntyped.Let
import TAPL2.Lib._


object FullRecon {

  trait Parser extends Recon.Parser with Let.Parser {

    val pFullReconE: PackratParser[Term] = pReconE ||| pLetE
    val pFullReconT: PackratParser[Ty] = pReconT

    override val pE: PackratParser[Term] = pFullReconE
    override val pT: PackratParser[Ty] = pFullReconT
  }

}

object TestFullRecon {

  def parseAndPrint(inp: String): Unit = {
    val p = new FullRecon.Parser {}
    println(parse(p.pE)(inp))
  }

}