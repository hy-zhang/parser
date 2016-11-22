package TAPLcomp2.fullsub

import scala.text.Document
import scala.text.Document._

// outer means that the term is the top-level term
object FullSubPrinter {

  import TAPLcomp2.Print._

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
    case TyVar(x) =>
      x
    case TyBool =>
      "Bool"
    case TyString =>
      "String"
    case TyUnit =>
      "Unit"
    case TyTop =>
      "Top"
    case TyRecord(fields) =>
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(false, tyTi)
        } else {
          g0(li :: ":" :/: ptyType(false, tyTi))
        }
      g2("{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.reduceLeftOption(_ :: "," :/:
        _).getOrElse(empty) :: "}")
    case TyNat =>
      "Nat"
    case TyFloat =>
      "Float"
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
    case TmAbs(x, tyT1, t2) =>
      val abs = g0("lambda" :/: x :: ":" :/: ptyType(false, tyT1) :: ".")
      val body = ptmTerm(outer, t2)
      g2(abs :/: body)
    case TmLet(x, t1, t2) =>
      g0("let " :: x :: " = " :: ptmTerm(false, t1) :/: "in" :/: ptmTerm(false, t2))
    case TmFix(t1) =>
      g2("fix " :: ptmTerm(false, t1))
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
      ptmPathTerm(outer, t)
  }

  def ptmPathTerm(outer: Boolean, t: Term): Document = t match {
    case TmProj(t1, l) =>
      ptmATerm(false, t1) :: "." :: l
    case t1 =>
      ptmAscribeTerm(outer, t1)
  }

  def ptmAscribeTerm(outer: Boolean, t: Term): Document = t match {
    case TmAscribe(t1, tyT1) =>
      g0(ptmAppTerm(false, t1) :/: "as " :: ptyType(false, tyT1))
    case t1 =>
      ptmATerm(outer, t1)
  }

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmInert(tyT) =>
      "inert[" :: ptyType(false, tyT) :: "]"
    case TmTrue =>
      "true"
    case TmFalse =>
      "false"
    case TmVar(x) =>
      x
    case TmString(s) =>
      "\"" :: s :: "\""
    case TmUnit =>
      "unit"
    case TmRecord(fields) =>
      def pf(i: Int, li: String, t: Term): Document =
        if (i.toString() == li) {
          ptmTerm(false, t)
        } else {
          li :: "=" :: ptmTerm(false, t)
        }
      "{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}"
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