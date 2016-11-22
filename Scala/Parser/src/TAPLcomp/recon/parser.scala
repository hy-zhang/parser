package TAPLcomp.recon

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty

case class TyVar(id: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyBool extends Ty

case object TyNat extends Ty

sealed trait Term

case class TmVar(i: String) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmAbs(v: String, ty: Option[Ty], t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term


object ReconParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero")
  lexical.delimiters += ("\\", "(", ")", ";", "/", ".", ":", "->", "=", "<", ">", "{", "}", "=>", "==>", ",", "|")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  // TYPES
  lazy val `type`: PackratParser[Ty] = arrowType
  lazy val aType: PackratParser[Ty] =
    "(" ~> `type` <~ ")" |
      ucid ^^ { tn => TyVar(tn) } |
      "Bool" ^^ { _ => TyBool } |
      "Nat" ^^ { _ => TyNat }

  lazy val fieldTypes: PackratParser[List[(String, Ty)]] =
    repsep(fieldType, ",")

  lazy val fieldType: PackratParser[(String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (id, ty) }

  lazy val arrowType: PackratParser[Ty] =
    (aType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => TyArr(t1, t2) } |
      aType

  lazy val term: PackratParser[Term] =
    appTerm |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => TmIf(t1, t2, t3) } |
      ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => TmAbs(v, Some(ty), t) }
  lazy val appTerm: PackratParser[Term] =
    appTerm ~ aTerm ^^ { case t1 ~ t2 => TmApp(t1, t2) } |
      "succ" ~> aTerm ^^ { t => TmSucc(t) } |
      "pred" ~> aTerm ^^ { t => TmPred(t) } |
      "iszero" ~> aTerm ^^ { t => TmIsZero(t) } |
      aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      "true" ^^ { _ => TmTrue } |
      "false" ^^ { _ => TmFalse } |
      lcid ^^ { i => TmVar(i) } |
      numericLit ^^ { x => num(x.toInt) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}