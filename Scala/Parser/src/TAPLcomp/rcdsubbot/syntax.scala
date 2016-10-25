package TAPLcomp.rcdsubbot

import scala.text.Document
import scala.text.Document._

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
    case TyRecord(fields) =>
      def pf(i: Int, li: String, tyTi: Ty): Document =
        if (i.toString() == li) {
          ptyType(false, tyTi)
        } else {
          g0(li :: ":" :/: ptyType(false, tyTi))
        }
      g2("{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}")
    case TyBot =>
      "Bot"
    case TyTop =>
      "Top"
    case tyT =>
      "(" :: ptyType(outer, tyT) :: ")"
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
      ptmPathTerm(outer, t)
  }

  def ptmPathTerm(outer: Boolean, t: Term): Document = t match {
    case TmProj(t1, l) =>
      ptmATerm(false, t1) :: "." :: l
    case t1 =>
      ptmATerm(outer, t1)
  }

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmVar(x) =>
      x
    case TmRecord(fields) =>
      def pf(i: Int, li: String, t: Term): Document =
        if (i.toString() == li) {
          ptmTerm(false, t)
        } else {
          li :: "=" :: ptmTerm(false, t)
        }
      "{" :: fields.zipWithIndex.map { case ((li, tyTi), i) => pf(i + 1, li, tyTi) }.
        reduceLeftOption(_ :: "," :/: _).getOrElse(empty) :: "}"
    case t =>
      "(" :: ptmTerm(outer, t) :: ")"
  }

  def ptm(t: Term) = ptmTerm(true, t)

}