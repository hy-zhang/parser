package TAPLcomp2.simplebool

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

sealed trait Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyBool extends Ty

object SimpleBoolParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else", "_")
  lexical.delimiters += ("(", ")", ";", "/", ".", ":", "->", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  lazy val `type`: PackratParser[Ty] = arrowType
  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |
      aType
  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |
      "Bool" ^^ { _ => TyBool }

  lazy val term: PackratParser[Term] =
    appTerm |
      ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, ty, t) } |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => TmIf(t1, t2, t3) }

  lazy val appTerm: PackratParser[Term] =
    (appTerm ~ aTerm) ^^ { case t1 ~ t2 => TmApp(t1, t2) } |
      aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => TmVar(i) } |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}