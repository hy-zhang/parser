package TAPL

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Util extends StandardTokenParsers with PackratParsers {
  lazy val lcid = ident ^? { case id if id.charAt(0).isLower => id }
  lazy val ucid = ident ^? { case id if id.charAt(0).isUpper => id }
  def fix[A](f: (=> A) => A): A = { lazy val a: A = f(a); a }
  def runParser(p : Parser[_]) : String => Unit = in => {
    phrase(p)(new lexical.Scanner(in)) match {
      case t if t.successful => println(t.get)
      case t                 => scala.sys.error(t.toString)
    }
  }
  implicit class Alternative[E, F, G, H](x : F => (=> H) => PackratParser[E]) {
    def |(y : G => (=> H) => PackratParser[E]) : (F with G) => (=> H) => PackratParser[E] =
      alg => l => this.x(alg)(l) ||| y(alg)(l)
  }
}