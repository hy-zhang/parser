package TAPL2.FullSimple

import TAPL2.FullUntyped.Record
import TAPL2.SimpleBool.Typed
import TAPL2.TyArith.TyArith
import TAPL2.FullUntyped.{Let, FloatString}
import TAPL2.Util._
import TAPL2.{Term, Ty}

case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyUnit extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TyVariant(els: List[(String, Ty)]) extends Ty

case object TyBool extends Ty

case object TyString extends Ty

case object TyNat extends Ty

case object TyFloat extends Ty

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TmCase(sel: Term, branches: List[(String, String, Term)]) extends Term

case class TmTag(tag: String, t: Term, ty: Ty) extends Term

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

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmInert(ty: Ty) extends Term
/* <6> */
object TypedRecord {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] extends Record.Parser[F] {
    lexical.delimiters += (":", ",", "{", "}")

    val pTypedRecordE: (=> F) => PackratParser[Term] = pRecordE

    val pTypedRecordT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TyRecord
    }
  }

}

object TypeVar {
  
  trait Parser[F <: {val pT : PackratParser[Ty]}] {
    val pTypeVarT: (=> F) => PackratParser[Ty] = l => ucid ^^ TyVar
  }

}

object Variant {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] {
    lexical.reserved += ("as", "case", "of")
    lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

    val pVariantE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE
      lazy val t = l.pT

      ("<" ~> lcid) ~ ("=" ~> e <~ ">") ~ ("as" ~> t) ^^ { case x ~ e0 ~ t0 => TmTag(x, e0, t0) } |||
        ("case" ~> e <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> e) ^^ { case x1 ~ x2 ~ e0 =>
          (x1, x2, e0)
        }, "|") ^^ { case e0 ~ l => TmCase(e0, l) }
    }

    val pVariantT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      "<" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ TyVariant
    }
  }

}

object Extension {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] {
    lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
    lexical.delimiters += ("(", ")", "[", "]")

    val pExtensionE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE
      lazy val t = l.pT

      "unit" ^^ { _ => TmUnit } |||
        e ~ ("as" ~> t) ^^ { case e0 ~ t0 => TmAscribe(e0, t0) } |||
        "fix" ~> e ^^ TmFix |||
        "inert" ~> "[" ~> t <~ "]" ^^ TmInert |||
        "(" ~> e <~ ")"
    }

    val pExtensionT: (=> F) => PackratParser[Ty] = l => {
      "Unit" ^^ { _ => TyUnit } |||
        "String" ^^ { _ => TyString } |||
        "Float" ^^ { _ => TyFloat }
    }
  }

}

// todo: rename; FullSimple - Variant
object Simple {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends TyArith.Parser[L] with Typed.Parser[L] with FloatString.Parser[L] with Let.Parser[L]
      with TypedRecord.Parser[L] with Extension.Parser[L] with TypeVar.Parser[L] {
    val pSimpleE: (=> L) => PackratParser[Term] =
      l => pTyArithE(l) ||| pTypedE(l) ||| pTypedRecordE(l) ||| pExtensionE(l) ||| pFloatStringE(l) ||| pLetE(l)
    val pSimpleT: (=> L) => PackratParser[Ty] =
      l => pTyArithT(l) ||| pTypedT(l) ||| pTypedRecordT(l) ||| pExtensionT(l) ||| pTypeVarT(l)
  }

}

object FullSimple {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Simple.Parser[L] with Variant.Parser[L] {
    val pFullSimpleE: (=> L) => PackratParser[Term] =
      l => pSimpleE(l) ||| pVariantE(l)
    val pFullSimpleT: (=> L) => PackratParser[Ty] =
      l => pSimpleT(l) ||| pVariantT(l)
  }

}

object TestFullSimple {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new FullSimple.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pFullSimpleE(l), lang.pFullSimpleT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}