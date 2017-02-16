package TAPL2.FullOmega

import TAPL2.FullSimple.Simple
import TAPL2.Lib._
import TAPL2.FullRef.Ref
import TAPL2.FullPoly.Pack


case object KnStar extends Kind

case class KnArr(k1: Kind, k2: Kind) extends Kind

case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TyRef(ty: Ty) extends Ty

case object TyString extends Ty

case object TyUnit extends Ty

case object TyBool extends Ty

case object TyNat extends Ty

case object TyFloat extends Ty

case class TyApp(ty1: Ty, ty2: Ty) extends Ty

case class TyAbs(v: String, k: Kind, ty: Ty) extends Ty

case class TySome(n: String, k: Kind, ty: Ty) extends Ty

case class TyAll(n: String, k: Kind, ty: Ty) extends Ty

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

case class TmUnPack(n1: String, n2: String, t1: Term, t2: Term) extends Term

case class TmTAbs(v: String, k: Kind, t: Term) extends Term

case class TmTApp(t: Term, ty: Ty) extends Term

case class TmLoc(i: Int) extends Term

case class TmRef(t: Term) extends Term

case class TmDeref(t: Term) extends Term

case class TmAssign(t1: Term, t2: Term) extends Term


object Omega {

  trait Parser extends ETKParser {
    lexical.reserved += ("Star", "All", "Some")
    lexical.delimiters += ("=>", ":", ".", ",", "{", "}")
    
    val pOmegaE: PackratParser[Term] =
      "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pE) ^^ { case x ~ kn ~ ex => TmTAbs(x, kn, ex) } |||
        pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => TmTApp(ex, ty) }

    val pOmegaT: PackratParser[Ty] =
      "All" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => TyAll(x, kn, ty) } |||
        ("{" ~> "Some" ~> ucid ~ (":" ~> pK) ~ ("," ~> pT) <~ "}") ^^ { case x ~ kn ~ ty => TySome(x, kn, ty) } |||
        "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => TyAbs(x, kn, ty) } |||
        pT ~ pT ^^ { case t1 ~ t2 => TyApp(t1, t2) }

    val pOmegaK: PackratParser[Kind] =
      "Star" ^^ { _ => KnStar } |||
        pK ~ ("=>" ~> pK) ^^ { case k1 ~ k2 => KnArr(k1, k2) } |||
        "(" ~> pK <~ ")"
  }

}

object FullOmega {

  trait Parser extends Simple.Parser with Pack.Parser with Ref.Parser with Omega.Parser {
    
    val pFullOmegaE: PackratParser[Term] = pSimpleE ||| pPackE ||| pRefE ||| pOmegaE
    val pFullOmegaT: PackratParser[Ty] = pSimpleT ||| pRefT ||| pOmegaT
    val pFullOmegaK: PackratParser[Kind] = pOmegaK

    override val pE: PackratParser[Term] = pFullOmegaE
    override val pT: PackratParser[Ty] = pFullOmegaT
    override val pK: PackratParser[Kind] = pFullOmegaK
  }

}

object TestFullOmega {

  def parseAndPrint(inp: String): Unit = {
    val p = new FullOmega.Parser {}
    println(parse(p.pE)(inp))
  }


}