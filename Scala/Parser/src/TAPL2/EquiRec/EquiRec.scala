package TAPL2.EquiRec

import TAPL2.FullSimple.TypeVar
import TAPL2.Lib._
import TAPL2.SimpleBool.Typed
import TAPL2.FullEquiRec.RecType


object EquiRec {

  trait Parser extends Typed.Parser with RecType.Parser with TypeVar.Parser {

    val pEquiRecE: PackratParser[Term] = pTypedE
    val pEquiRecT: PackratParser[Ty] = pTypedT ||| pRecTypeT ||| pTypeVarT

    override val pE: PackratParser[Term] = pEquiRecE
    override val pT: PackratParser[Ty] = pEquiRecT
  }

}

object TestEquiRec {

  def parseAndPrint(inp: String): Unit = {
    val p = new EquiRec.Parser {}
    println(parse(p.pE)(inp))
  }

}