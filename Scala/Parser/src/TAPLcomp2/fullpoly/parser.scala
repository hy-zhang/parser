package TAPLcomp2.fullpoly

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty

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

sealed trait Term

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

case class TmTAbs(v: String, t: Term) extends Term

case class TmTApp(t: Term, ty: Ty) extends Term

object FullPolyParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("lambda", "Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "letrec", "_", "All", "Some")
  lexical.delimiters += ("(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|", "*",
    "[", "]", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  // TYPES
  lazy val `type`: PackratParser[Ty] =
  arrowType |||
    ("All" ~> ucid) ~ ("." ~> `type`) ^^ { case id ~ ty => TyAll(id, ty) }
  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |||
      ucid ^^ { tn => TyVar(tn) } |||
      "Bool" ^^ { _ => TyBool } |||
      "String" ^^ { _ => TyString } |||
      "Unit" ^^ { _ => TyUnit } |||
      "{" ~> fieldTypes <~ "}" ^^ { ft => TyRecord(ft) } |||
      "Nat" ^^ { _ => TyNat } |||
      "Float" ^^ { _ => TyFloat } |||
      (("{" ~ "Some") ~> ucid) ~ ("," ~> `type` <~ "}") ^^ { case id ~ ty => TySome(id, ty) }

  lazy val fieldTypes: PackratParser[List[(String, Ty)]] =
    repsep(fieldType, ",")

  lazy val fieldType: PackratParser[(String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (id, ty) }

  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |||
      aType

  // TERMS
  lazy val term: PackratParser[Term] =
  appTerm |||
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => TmIf(t1, t2, t3) } |||
    (("lambda" ||| "\\") ~> (lcid ||| "_")) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, ty, t) } |||
    ("let" ~> (lcid ||| "_")) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => TmLet(id, t1, t2) } ||| {
    (("let" ~ "{") ~> ucid) ~ ("," ~> lcid <~ "}") ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id1 ~ id2 ~ t1 ~ t2 =>
      TmUnPack(id1, id2, t1, t2) }
  } |||
    (("lambda" ||| "\\") ~> ucid) ~ ("." ~> term) ^^ { case id ~ t => TmTAbs(id, t) }

  lazy val appTerm: PackratParser[Term] =
    (appTerm <~ "[") ~ (`type` <~ "]") ^^ { case t ~ ty => TmTApp(t, ty) } |||
      appTerm ~ pathTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |||
      "fix" ~> pathTerm ^^ { t => TmFix(t) } |||
      "succ" ~> pathTerm ^^ { t => TmSucc(t) } |||
      "pred" ~> pathTerm ^^ { t => TmPred(t) } |||
      "iszero" ~> pathTerm ^^ { t => TmIsZero(t) } |||
      pathTerm

  lazy val ascribeTerm: PackratParser[Term] =
    aTerm ~ ("as" ~> `type`) ^^ { case t ~ ty => TmAscribe(t, ty) } |||
      aTerm

  lazy val pathTerm: PackratParser[Term] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => TmProj(t1, l) } |||
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => TmProj(t1, l) } |||
      ascribeTerm

  lazy val termSeq: PackratParser[Term] =
    term ~ (";" ~> termSeq) ^^ { case t ~ ts => TmApp(TmAbs("_", TyUnit, ts), t) } |||
      term

  lazy val aTerm: PackratParser[Term] =
    "(" ~> termSeq <~ ")" |||
      ("inert" ~ "[") ~> `type` <~ "]" ^^ { ty => TmInert(ty) } |||
      "true" ^^ { _ => TmTrue } |||
      "false" ^^ { _ => TmFalse } |||
      lcid ^^ { i => TmVar(i) } |||
      stringLit ^^ { l => TmString(l) } |||
      "unit" ^^ { _ => TmUnit } |||
      "{" ~> fields <~ "}" ^^ { fs => TmRecord(fs) } |||
      numericLit ^^ { x => num(x.toInt) } |||
      (("{" ~ "*") ~> `type`) ~ ("," ~> term <~ "}") ~ ("as" ~> `type`) ^^ { case ty1 ~ t ~ ty2 => TmPack(ty1, t, ty2) }

  lazy val fields: PackratParser[List[(String, Term)]] =
    repsep(field, ",")
  lazy val field: PackratParser[(String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (id, t) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}