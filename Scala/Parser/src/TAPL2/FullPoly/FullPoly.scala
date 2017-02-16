package TAPL2.FullPoly

import TAPL2.FullSimple.Simple
import TAPL2.Lib._


case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyString extends Ty

case object TyUnit extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case object TyBool extends Ty

case object TyNat extends Ty

case object TyFloat extends Ty

case class TySome(n: String, ty: Ty) extends Ty

case class TyAll(n: String, ty: Ty) extends Ty


case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case class TmLet(l: String, t1: Term, t2: Term) extends Term

case class TmFix(t: Term) extends Term

case class TmString(s: String) extends Term

case object TmUnit extends Term

case class TmAscribe(t: Term, ty: Ty) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmInert(ty: Ty) extends Term

case class TmPack(ty: Ty, t: Term, as: Ty) extends Term

case class TmUnpack(n1: String, n2: String, t1: Term, t2: Term) extends Term

case class TmTAbs(v: String, t: Term) extends Term

case class TmTApp(t: Term, ty: Ty) extends Term


object Pack {

  trait Parser extends ETParser {
    lexical.reserved += ("as", "let", "in")
    lexical.delimiters += (",", "{", "}", "*", "=")

    val pPackE: PackratParser[Term] =
      ("{" ~> "*" ~> pT ~ ("," ~> pE) <~ "}") ~ ("as" ~> pT) ^^ { case t1 ~ ex ~ t2 => TmPack(t1, ex, t2) } |||
        "let" ~> ("{" ~> ucid ~ ("," ~> lcid) <~ "}") ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ { case tx ~ x ~ e1 ~ e2 =>
          TmUnpack(tx, x, e1, e2) }
  }

}

object Poly {

  trait Parser extends ETParser {
    lexical.reserved += ("All", "Some")
    lexical.delimiters += (".", ",", "{", "}", "[", "]")

    val pPolyE: PackratParser[Term] =
      "\\" ~> ucid ~ ("." ~> pE) ^^ { case x ~ ex => TmTAbs(x, ex) } |||
        pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => TmTApp(ex, ty) }

    val pPolyT: PackratParser[Ty] =
      "All" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => TyAll(x, ty) } |||
        ("{" ~> "Some" ~> ucid ~ ("," ~> pT) <~ "}") ^^ { case x ~ ty => TySome(x, ty) }
  }

}

object FullPoly {

  trait Parser extends Simple.Parser with Poly.Parser with Pack.Parser {

    val pFullPolyE: PackratParser[Term] = pSimpleE ||| pPolyE ||| pPackE
    val pFullPolyT: PackratParser[Ty] = pSimpleT ||| pPolyT

    override val pE: PackratParser[Term] = pFullPolyE
    override val pT: PackratParser[Ty] = pFullPolyT
  }

}

object TestFullPoly {

  def parseAndPrint(inp: String): Unit = {
    val p = new FullPoly.Parser {}
    println(parse(p.pE)(inp))
  }

}