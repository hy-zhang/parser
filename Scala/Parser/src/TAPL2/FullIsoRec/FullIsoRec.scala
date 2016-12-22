package TAPL2.FullIsoRec

import TAPL2.FullEquiRec.FullEquiRec
import TAPL2.Util._
import TAPL2.{Term, Ty}

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

case class TmFold(ty: Ty) extends Term

case class TmUnfold(ty: Ty) extends Term
/*
/* <13> */
object Fold {
  
  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] {
    lexical.reserved += ("fold", "unfold")
    lexical.delimiters += ("[", "]")

    val pFoldE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        "fold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => TmFold(ex, ty) },
        "unfold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => TmUnfold(ex, ty) }
      ).reduce((a, b) => a ||| b)
    }
  }

}

object FullIsoRec {

  trait Parser[L <: {val pE : PackratParser[E]; val pT : PackratParser[T]}]
    extends FullEquiRec.Parser[L] with Fold.Parser[L] {
    val pFullIsoRecE = pFullEquiRecE | pFoldE
    val pFullIsoRecT = pFullEquiRecT
  }

}

object TestFullIsoRec {

  import _

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullIsoRec.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullIsoRec.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullIsoRecE(alg)(l), lang.pFullIsoRecT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullIsoRec.Print {})

}*/