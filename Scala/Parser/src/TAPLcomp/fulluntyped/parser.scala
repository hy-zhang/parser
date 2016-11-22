package TAPLcomp.fulluntyped

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

case class TmString(s: String) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmLet(l: String, t1: Term, t2: Term) extends Term

object FullUntypedParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "iszero", "letrec", "_")
  lexical.delimiters += ("(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  lazy val term: PackratParser[Term] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => TmIf(t1, t2, t3) } |
      ("\\" ~> lcid) ~ ("." ~> term) ^^ { case v ~ t => TmAbs(v, t) } |
      ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => TmLet(id, t1, t2) }
  lazy val appTerm: PackratParser[Term] =
    appTerm ~ pathTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |
      "succ" ~> pathTerm ^^ { t => TmSucc(t) } |
      "pred" ~> pathTerm ^^ { t => TmPred(t) } |
      "iszero" ~> pathTerm ^^ { t => TmIsZero(t) } |
      pathTerm

  lazy val pathTerm: PackratParser[Term] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => TmProj(t1, l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => TmProj(t1, l) } |
      aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      lcid ^^ { i => TmVar(i) } |
      stringLit ^^ { l => TmString(l) } |
      "{" ~> fields <~ "}" ^^ { fs => TmRecord(fs) } |
      numericLit ^^ { x => num(x.toInt) }

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