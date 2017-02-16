package TAPL2.SimpleBool

import TAPL2.Lib._
import TAPL2.TyArith.TypedBool
import TAPL2.Untyped.VarApp


case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyBool extends Ty


object Typed {

  trait Parser extends ETParser with VarApp.Parser {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")
    
    private val pAbsE: PackratParser[Term] =
      ("\\" ~> lcid) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => TmAbs(x, t0, e0) }

    val pTypedE: PackratParser[Term] = pVarAppE ||| pAbsE

    val pTypedT: PackratParser[Ty] =
      pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => TyArr(t1, t2) } |||
        "(" ~> pT <~ ")"
  }

}

object SimpleBool {

  trait Parser extends Typed.Parser with TypedBool.Parser {

    val pSimpleBoolE: PackratParser[Term] = pTypedE ||| pTypedBoolE
    val pSimpleBoolT: PackratParser[Ty] = pTypedT ||| pTypedBoolT

    override val pE: PackratParser[Term] = pSimpleBoolE
    override val pT: PackratParser[Ty] = pSimpleBoolT
  }

}

object TestSimpleBool {
  def parseAndPrint(inp: String): Unit = {
    val p = new SimpleBool.Parser {}
    println(parse(p.pE)(inp))
  }
}