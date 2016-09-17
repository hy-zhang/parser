package TAPL

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Util extends StandardTokenParsers with PackratParsers {
  def fix[A](f: (=> A) => A): A = { lazy val a: A = f(a); a }
  def runParser(p : Parser[_]) : String => Unit = in => {
    phrase(p)(new lexical.Scanner(in)) match {
      case t if t.successful => println(t.get)
      case t                 => scala.sys.error(t.toString)
    }
  }
}