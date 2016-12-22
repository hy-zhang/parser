package PaperCode.Overview

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Util extends StandardTokenParsers with PackratParsers {

  type Open[T] = (=> T) => T
  
  type Parser[T] = PackratParser[T]

  implicit class Alternative[E, F, G, H](x: F => (=> H) => PackratParser[E]) {
    def |(y: G => (=> H) => PackratParser[E]): (F with G) => (=> H) => PackratParser[E] =
      alg => l => this.x(alg)(l) ||| y(alg)(l)
  }

  def fix[T](f: Open[T]): T = { lazy val a: T = f(a); a }

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }
}
