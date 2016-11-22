package TAPLcomp2.tyarith

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Ty

case object TyBool extends Ty

case object TyNat extends Ty

sealed trait Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

object TyArithParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
  lexical.delimiters += ("(", ")", ";")

  private def term: Parser[Term] = appTerm |||
    ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ TmIf

  private def appTerm: Parser[Term] =
    aTerm |||
      "succ" ~> aTerm ^^ TmSucc |||
      "pred" ~> aTerm ^^ TmPred |||
      "iszero" ~> aTerm ^^ TmIsZero

  //  Atomic terms are ones that never require extra parentheses
  private def aTerm: Parser[Term] =
  "(" ~> term <~ ")" |||
    "true" ^^ { _ => TmTrue } |||
    "false" ^^ { _ => TmFalse } |||
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