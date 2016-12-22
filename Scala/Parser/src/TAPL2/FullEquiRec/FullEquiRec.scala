package TAPL2.FullEquiRec

import TAPL2.FullSimple.FullSimple
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

/* <12> */
object RecType {

  trait Parser[F <: {val pT : PackratParser[Ty]}] {
    lexical.reserved += "Rec"
    lexical.delimiters += "."

    val pRecTypeT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      "Rec" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => TyRec(x, ty) }
    }
  }

}

object FullEquiRec {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends FullSimple.Parser[L] with RecType.Parser[L] {
    val pFullEquiRecE: (=> L) => PackratParser[Term] = pFullSimpleE
    val pFullEquiRecT: (=> L) => PackratParser[Ty] =
      l => pFullSimpleT(l) ||| pRecTypeT(l)
  }

}

object TestFullEquiRec {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new FullEquiRec.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pFullEquiRecE(l), lang.pFullEquiRecT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}