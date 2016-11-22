package TAPLcomp2.equirec

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty

case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case class TyRec(id: String, ty: Ty) extends Ty

sealed trait Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

object EquiRecParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += "Rec"
  lexical.delimiters += ("(", ")", ";", ".", ":", "->", "=", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  // TYPES
  lazy val `type`: PackratParser[Ty] =
  arrowType |
    ("Rec" ~> ucid) ~ ("." ~> `type`) ^^ { case id ~ ty => TyRec(id, ty) }

  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |
      ucid ^^ { tn => TyVar(tn) }

  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |
      aType

  // TERMS
  lazy val term: PackratParser[Term] =
  appTerm |
    ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, ty, t) }

  lazy val appTerm: PackratParser[Term] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |
      aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => TmVar(i) }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}