package TAPL2.Recon

import TAPL2.FullSimple.TypeVar
import TAPL2.Lib._
import TAPL2.SimpleBool.Typed
import TAPL2.TyArith.TyArith


object Recon {
  
  trait Parser extends TyArith.Parser with Typed.Parser with TypeVar.Parser {

    val pReconE: PackratParser[Term] = pTypedE ||| pTyArithE
    val pReconT: PackratParser[Ty] = pTypedT ||| pTyArithT ||| pTypeVarT

    override val pE: PackratParser[Term] = pReconE
    override val pT: PackratParser[Ty] = pReconT
  }

}

object TestRecon {

  def parseAndPrint(inp: String): Unit = {
    val p = new Recon.Parser {}
    println(parse(p.pE)(inp))
  }

}