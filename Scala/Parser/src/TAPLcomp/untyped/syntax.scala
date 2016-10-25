package TAPLcomp.untyped

import scala.text.Document

// outer means that the term is the top-level term
object PrettyPrinter {
  import TAPLcomp.Print._

  def ptmTerm(outer: Boolean, t: Term): Document = t match {
    case TmAbs(x, t2) =>
      val abs = g0("\\" :: x :: ".")
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

  def ptm(t: Term) = ptmTerm(true, t)

  def ptmATerm(outer: Boolean, t: Term): Document = t match {
    case TmVar(x) => x
    case t => "(" :: ptmTerm(outer, t) :: ")"
  }

}