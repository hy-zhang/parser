package papercode

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers


trait Common extends StandardTokenParsers with PackratParsers {
  val lcid: Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid: Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  type Parser[E] = PackratParser[E]

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    t.getOrElse(sys.error(t.toString))
  }
}
