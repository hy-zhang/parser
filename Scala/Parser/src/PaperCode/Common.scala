package PaperCode

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._


trait Common extends StandardTokenParsers with PackratParsers {

  type Parser[E] = PackratParser[E]

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

}
