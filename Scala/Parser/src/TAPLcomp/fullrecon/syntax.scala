package TAPLcomp.fullrecon

import scala.text.Document

// outer means that the term is the top-level term
object PrettyPrinter {

  import TAPLcomp.Print._

  def ptyType(outer: Boolean, ty: Ty): Document = ty match {
    case ty => ptyArrowType(outer, ty)
  }

  def ptyArrowType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyArr(tyT1, tyT2) =>
      g2(ptyAType(false, tyT1) :: " ->" :/: ptyArrowType(outer, tyT2))
    case tyT =>
      ptyAType(outer, tyT)
  }

  def ptyAType(outer: Boolean, tyT: Ty): Document = tyT match {
    case TyVar(b) =>
      b
    case TyBool =>
      "Bool"
    case TyNat =>
      "Nat"
    case tyT =>
      "(" :: ptyType(outer, tyT) :: ")"
  }

  def ptyTy(ty: Ty) = ptyType(true, ty)

  def ptmTerm(outer: Boolean, t: Term): Document = t match {

    case TmIf(t1, t2, t3) =>
      val ifB = g2("if" :/: ptmTerm(outer, t1))
      val thenB = g2("then" :/: ptmTerm(outer, t2))
      val elseB = g2("else" :/: ptmTerm(outer, t3))
      g0(ifB :/: thenB :/: elseB)
    case TmAbs(x, Some(tyT1), t2) =>
      val abs = g0("lambda" :/: x :: ":" :/: ptyType(false, tyT1) :: ".")
      val body = ptmTerm(outer, t2)
      g2(abs :/: body)
    case TmAbs(x, None, t2) =>
      val abs = g0("lambda" :/: x :: ".")
      val body = ptmTerm(outer, t2)
      g2(abs :/: body)
    case TmLet(x, t1, t2) =>
      g0("let " :: x :: " = " :: ptmTerm(false, t1) :/: "in" :/: ptmTerm(false, t2))
    case t => ptmAppTerm(outer, t)
  }

  def ptmAppTerm(outer: Boolean, t: Term): Document = t match {
    case TmApp(t1, t2) =>
      g2(ptmAppTerm(false, t1) :/: ptmATerm(false, t2))
    case TmPred(t1) =>
      "pred " :: ptmATerm(false, t1)
    case TmIsZero(t1) =>
      "iszero " :: ptmATerm(false, t1)
    case t =>
      ptmATerm(outer, t)
  }

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmTrue =>
      "true"
    case TmFalse =>
      "false"
    case TmVar(x) =>
      x
    case TmZero =>
      "0"
    case TmSucc(t1) =>
      def pf(i: Int, t: Term): Document = t match {
        case TmZero =>
          i.toString()
        case TmSucc(s) =>
          pf(i + 1, s)
        case _ =>
          "(succ " :: ptmATerm(false, t1) :: ")"
      }
      pf(1, t1)
    case t =>
      "(" :: ptmTerm(outer, t) :: ")"
  }

  def ptm(t: Term) = ptmTerm(true, t)

}