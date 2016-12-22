package TAPL2

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Util extends StandardTokenParsers with PackratParsers {
  val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  val ucid = ident ^? { case id if id.charAt(0).isUpper => id }

  implicit class Alternative[E, F, G, H](x: F => (=> H) => PackratParser[E]) {
    def |(y: G => (=> H) => PackratParser[E]): (F with G) => (=> H) => PackratParser[E] =
      alg => l => this.x(alg)(l) ||| y(alg)(l)
  }

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  def runParser(p: Parser[_]): String => Unit = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }
}