package TAPLcomp.rcdsubbot

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty
case object TyTop extends Ty
case object TyBot extends Ty
case class TyArr(t1: Ty, t2: Ty) extends Ty
case class TyRecord(els: List[(String, Ty)]) extends Ty

sealed trait Term
case class TmVar(i: String) extends Term
case class TmAbs(v: String, ty: Ty, t: Term) extends Term
case class TmApp(t1: Term, t2: Term) extends Term
case class TmRecord(fields: List[(String, Term)]) extends Term
case class TmProj(t: Term, proj: String) extends Term

object RcdSubBotParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "Top", "Bot")
  lexical.delimiters += ("\\", "(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  lazy val `type`: PackratParser[Ty] = arrowType
  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |
      "Bot" ^^ { _ => TyBot } |
      "Top" ^^ { _ => TyTop } |
      "{" ~> fieldTypes <~ "}" ^^ { ft => TyRecord(ft) }

  lazy val fieldTypes: PackratParser[List[(String, Ty)]] =
    repsep(fieldType, ",")

  lazy val fieldType: PackratParser[(String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (id, ty) }

  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |
      aType

  // TERMS
  lazy val term: PackratParser[Term] =
    appTerm |
      ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, ty, t) }
  lazy val appTerm: PackratParser[Term] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |
      pathTerm

  lazy val pathTerm: PackratParser[Term] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => TmProj(t1, l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => TmProj(t1, l) } |
      aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => TmVar(i) } |
      "{" ~> fields <~ "}" ^^ { fs => TmRecord(fs) }

  lazy val fields: PackratParser[List[(String, Term)]] =
    repsep(field, ",")
  lazy val field: PackratParser[(String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (id, t) }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t                 => error(t.toString)
  }

}