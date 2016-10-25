package TAPLcomp.equirec

import scala.text.Document
import scala.text.Document._

// outer means that the term is the top-level term
object PrettyPrinter {
  import TAPLcomp.Print._

  def ptyType(outer: Boolean, ty: Ty): Document = ty match {
    case TyRec(x, tyT) =>
      g2("Rec " :: x :: "." :/: ptyType(outer, tyT))
    case ty =>
      ptyArrowType(outer, ty)
  }

  def ptyArrowType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyArr(tyT1, tyT2) =>
      g2(ptyAType(false, tyT1) :: " ->" :/: ptyArrowType(outer, tyT2))
    case tyT =>
      ptyAType(outer, tyT)
  }

  def ptyAType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyVar(x) => x
    case tyT => "(" :: ptyType(outer, tyT) :: ")"
  }

  def ptyTy(ty: Ty) = ptyType(true, ty)

  def ptmTerm(outer: Boolean, t: Term): Document = t match {
    case TmAbs(x, tyT1, t2) =>
      val abs = g0("lambda" :/: x :: ":" :/: ptyType(false, tyT1) :: ".")
      val body = ptmTerm(outer, t2)
      g2(abs :/: body)
    case t => ptmAppTerm(outer, t)
  }

  def ptmAppTerm(outer: Boolean, t: Term): Document = t match {
    case TmApp(t1, t2) =>
      g2(ptmAppTerm(false, t1) :/: ptmATerm(false, t2))
    case t =>
      ptmATerm(outer, t)
  }

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmVar(x) =>
      x
    case t =>
      "(" :: ptmTerm(outer, t) :: ")"
  }

  def ptm(t: Term) = ptmTerm(true, t)

}