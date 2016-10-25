package TAPLcomp.simplebool

import scala.text.Document

// outer means that the term is the top-level term
object SimpleBoolPrinter {

  import TAPLcomp.Print._

  def ptyType(outer: Boolean, ty: Ty): Document = ty match {
    case ty => ptyArrowType(outer, ty)
  }

  def ptyArrowType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyArr(tyT1, tyT2) =>
      g0(ptyAType(false, tyT1) :: " ->" :/: ptyArrowType(outer, tyT2))
    case tyT =>
      ptyAType(outer, tyT)
  }

  def ptyAType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyBool => "Bool"
    case tyT => "(" :: ptyType(outer, tyT) :: ")"
  }

  def ptyTy(ty: Ty) = ptyType(true, ty)

  def ptmTerm(outer: Boolean, t: Term): Document = t match {
    case TmAbs(x, tyT1, t2) =>
      val abs = g0("\\" :: x :: ":" :: ptyType(false, tyT1) :: ".")
      val body = ptmTerm(outer, t2)
      g2(abs :/: body)
    case TmIf(t1, t2, t3) =>
      val ifB = g2("if" :/: ptmTerm(outer, t1))
      val thenB = g2("then" :/: ptmTerm(outer, t2))
      val elseB = g2("else" :/: ptmTerm(outer, t3))
      g0(ifB :/: thenB :/: elseB)
    case t => ptmAppTerm(outer, t)

  }

  def ptmAppTerm(outer: Boolean, t: Term): Document = t match {
    case TmApp(t1, t2) =>
      g2(ptmAppTerm(false, t1) :/: ptmATerm(false, t2))
    case t =>
      ptmATerm(outer, t)
  }

  def ptm(t: Term) = ptmTerm(true, t)

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmVar(x) =>
      x
    case TmTrue =>
      "true"
    case TmFalse =>
      "false"
    case t =>
      "(" :: ptmTerm(outer, t) :: ")"
  }

}