package TAPL2.FullSimple

import TAPL2.Lib._
import TAPL2.FullUntyped.{FloatString, Let, Record}
import TAPL2.SimpleBool.Typed
import TAPL2.TyArith.TyArith


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


object TypedRecord {

  trait Parser extends ETParser with Record.Parser {
    lexical.delimiters += (":", ",", "{", "}")

    val pTypedRecordE: PackratParser[Term] = pRecordE

    val pTypedRecordT: PackratParser[Ty] =
      "{" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TyRecord

  }

}

object TypeVar {

  trait Parser {
    val pTypeVarT: PackratParser[Ty] = ucid ^^ TyVar
  }

}

object Variant {

  trait Parser extends ETParser {
    lexical.reserved += ("as", "case", "of")
    lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")
    
    val pVariantE: PackratParser[Term] = {
      ("<" ~> lcid) ~ ("=" ~> pE <~ ">") ~ ("as" ~> pT) ^^ { case x ~ e0 ~ t0 => TmTag(x, e0, t0) } |||
        ("case" ~> pE <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> pE) ^^ { case x1 ~ x2 ~ e0 =>
          (x1, x2, e0)
        }, "|") ^^ { case e0 ~ l => TmCase(e0, l) }
    }

    val pVariantT: PackratParser[Ty] =
      "<" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ TyVariant
  }

}

object Extension {

  trait Parser extends ETParser {
    lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
    lexical.delimiters += ("(", ")", "[", "]")

    val pExtensionE: PackratParser[Term] = {
      "unit" ^^ { _ => TmUnit } |||
        pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => TmAscribe(e0, t0) } |||
        "fix" ~> pE ^^ TmFix |||
        "inert" ~> "[" ~> pT <~ "]" ^^ TmInert
    }

    val pExtensionT: PackratParser[Ty] = {
      "Unit" ^^ { _ => TyUnit } |||
        "String" ^^ { _ => TyString } |||
        "Float" ^^ { _ => TyFloat }
    }
  }

}

// todo: rename; FullSimple - Variant
object Simple {

  trait Parser extends TyArith.Parser with Typed.Parser with FloatString.Parser
    with Let.Parser with TypedRecord.Parser with Extension.Parser with TypeVar.Parser {

    val pSimpleE: PackratParser[Term] =
      pTyArithE ||| pTypedE ||| pTypedRecordE ||| pExtensionE ||| pFloatStringE ||| pLetE

    val pSimpleT: PackratParser[Ty] =
      pTyArithT ||| pTypedT ||| pTypedRecordT ||| pExtensionT ||| pTypeVarT
  }

}

object FullSimple {

  trait Parser extends Simple.Parser with Variant.Parser {

    val pFullSimpleE: PackratParser[Term] = pSimpleE ||| pVariantE
    val pFullSimpleT: PackratParser[Ty] = pSimpleT ||| pVariantT

    override val pE: PackratParser[Term] = pFullSimpleE

    override val pT: PackratParser[Ty] = pFullSimpleT
  }

}

object TestFullSimple {
  def parseAndPrint(inp: String): Unit = {
    val p = new FullSimple.Parser {}
    println(parse(p.pE)(inp))
  }
}