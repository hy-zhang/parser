package TAPLcomp.untyped

import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed trait Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

// This parser is done exactly in the same way as in TAPL.
// The oddity of this parser (elementary parsers return functions) is driven by the desire
// to avoid double hierarchy of terms (named and nameless). 
// The input text represents named terms. The module works with nameless terms 
// So translation from named form into nameless form is done on the fly during parsing.
object UntypedParsers extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("_")
  lexical.delimiters += ("(", ")", ";", "/", ".", "\\")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  lazy val term: PackratParser[Term] =
    appTerm |
      ("\\" ~> lcid) ~ ("." ~> term) ^^ { case v ~ t => TmAbs(v, t) }

  lazy val appTerm: PackratParser[Term] =
    (appTerm ~ aTerm) ^^ { case t1 ~ t2 => TmApp(t1, t2) } | aTerm

  lazy val aTerm: PackratParser[Term] =
    "(" ~> term <~ ")" |
      lcid ^^ { i => TmVar(i) }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }
}