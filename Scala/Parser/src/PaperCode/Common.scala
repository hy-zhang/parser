package PaperCode

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._


trait Common extends StandardTokenParsers with PackratParsers {

  type Parser[E] = PackratParser[E]

  val lcid: Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid: Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

}
