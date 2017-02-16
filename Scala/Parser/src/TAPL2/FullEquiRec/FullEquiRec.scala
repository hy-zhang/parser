package TAPL2.FullEquiRec

import TAPL2.Lib._
import TAPL2.FullSimple.FullSimple


case class TyVar(i: String) extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyNat extends Ty

case class TyVariant(els: List[(String, Ty)]) extends Ty

case object TyBool extends Ty

case object TyString extends Ty

case object TyFloat extends Ty

case object TyUnit extends Ty

case class TyRec(id: String, ty: Ty) extends Ty

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TmVar(i: String) extends Term

case class TmString(s: String) extends Term

case class TmAscribe(t: Term, ty: Ty) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmInert(ty: Ty) extends Term

case class TmCase(sel: Term, branches: List[(String, String, Term)]) extends Term

case class TmTag(tag: String, t: Term, ty: Ty) extends Term

case class TmLet(l: String, t1: Term, t2: Term) extends Term

case object TmUnit extends Term

case class TmFix(t: Term) extends Term


object RecType {

  trait Parser extends TParser {
    lexical.reserved += "Rec"
    lexical.delimiters += "."

    val pRecTypeT: PackratParser[Ty] = "Rec" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => TyRec(x, ty) }
  }

}

object FullEquiRec {
  
  trait Parser extends FullSimple.Parser with RecType.Parser {

    val pFullEquiRecE: PackratParser[Term] = pFullSimpleE
    val pFullEquiRecT: PackratParser[Ty] = pFullSimpleT ||| pRecTypeT

    override val pE: PackratParser[Term] = pFullEquiRecE
    override val pT: PackratParser[Ty] = pFullEquiRecT
  }

}

object TestFullEquiRec {

  def parseAndPrint(inp: String): Unit = {
    val p = new FullEquiRec.Parser {}
    println(parse(p.pE)(inp))
  }

}