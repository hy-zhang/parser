package TAPLcomp2.fullref

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty

case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyUnit extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TyVariant(els: List[(String, Ty)]) extends Ty

case object TyBool extends Ty

case object TyString extends Ty

case object TyFloat extends Ty

case object TyNat extends Ty

case class TyRef(ty: Ty) extends Ty

case object TyTop extends Ty

case object TyBot extends Ty

case class TySource(ty: Ty) extends Ty

case class TySink(ty: Ty) extends Ty

sealed trait Term

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

case class TmLoc(i: Int) extends Term

case class TmRef(t: Term) extends Term

case class TmDeref(t: Term) extends Term

case class TmAssign(t1: Term, t2: Term) extends Term

object FullRefParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "Top", "Bot", "Ref", "Source", "Sink", "ref", "inert")
  lexical.delimiters += ("\\", "(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", ",", "|",
    "!", ":=", "[", "]")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  // TYPES
  lazy val `type`: PackratParser[Ty] = arrowType |||
    "Ref" ~> aType ^^ { ty => TyRef(ty) } |||
    "Source" ~> aType ^^ { ty => TySource(ty) } |||
    "Sink" ~> aType ^^ { ty => TySink(ty) }
  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |||
      ucid ^^ { tn => TyVar(tn) } |||
      "Bool" ^^ { _ => TyBool } |||
      "Top" ^^ { _ => TyTop } |||
      "Bot" ^^ { _ => TyBot } |||
      "<" ~> fieldTypes <~ ">" ^^ { ft => TyVariant(ft) } |||
      "String" ^^ { _ => TyString } |||
      "Unit" ^^ { _ => TyUnit } |||
      "{" ~> fieldTypes <~ "}" ^^ { ft => TyRecord(ft) } |||
      "Nat" ^^ { _ => TyNat } |||
      "Float" ^^ { _ => TyFloat }

  lazy val fieldTypes: PackratParser[List[(String, Ty)]] =
    repsep(fieldType, ",")

  lazy val fieldType: PackratParser[(String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (id, ty) }

  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |||
      aType

  // TERMS
  lazy val term: PackratParser[Term] =
  ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => TmIf(t1, t2, t3) } |||
    ("case" ~> term) ~ ("of" ~> cases) ^^ { case t ~ cs => TmCase(t, cs) } |||
    ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, ty, t) } |||
    ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => TmLet(id, t1, t2) } |||
    (appTerm <~ ":=") ~ appTerm ^^ { case t1 ~ t2 => TmAssign(t1, t2) } |||
    appTerm

  lazy val appTerm: PackratParser[Term] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |||
      "fix" ~> pathTerm ^^ { t => TmFix(t) } |||
      "ref" ~> pathTerm ^^ { t => TmRef(t) } |||
      "!" ~> pathTerm ^^ { t => TmDeref(t) } |||
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
      ("<" ~> lcid) ~ ("=" ~> term <~ ">") ~ ("as" ~> `type`) ^^ { case l ~ t ~ ty => TmTag(l, t, ty) } |||
      lcid ^^ { i => TmVar(i) } |||
      stringLit ^^ { l => TmString(l) } |||
      "unit" ^^ { _ => TmUnit } |||
      "{" ~> fields <~ "}" ^^ { fs => TmRecord(fs) } |||
      numericLit ^^ { x => num(x.toInt) }

  lazy val cases: PackratParser[List[(String, String, Term)]] =
    rep1sep(`case`, "|")
  lazy val `case`: PackratParser[(String, String, Term)] =
    ("<" ~> lcid <~ "=") ~ (lcid <~ ">") ~ ("=>" ~> term) ^^ { case l1 ~ l2 ~ t => (l1, l2, t) }

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