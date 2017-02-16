package TAPL2

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._


object Lib extends StandardTokenParsers with PackratParsers {

  val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  type Open[T] = (=> T) => T

  def parse[E](p: PackratParser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

  abstract class Term

  abstract class Ty

  abstract class Kind

  trait EParser {
    val pE: PackratParser[Term]
  }

  trait TParser {
    val pT: PackratParser[Ty]
  }

  trait ETParser extends EParser with TParser

  trait KParser {
    val pK: PackratParser[Kind]
  }

  trait ETKParser extends ETParser with KParser
}
