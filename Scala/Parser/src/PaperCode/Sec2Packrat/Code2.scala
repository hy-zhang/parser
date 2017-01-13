package PaperCode.Sec2Packrat

import PaperCode.Common


object Code2 extends Common {

  // Abstract syntax
  trait Expr {
    def print: String
  }

  class Lit(x: Int) extends Expr {
    def print = x.toString
  }

  class Add(e1: Expr, e2: Expr) extends Expr {
    def print = "(" + e1.print + " + " + e2.print + ")"
  }

  // Parser with lexing
  trait ExprParser {
    lexical.delimiters += "+"
    val pLit: PackratParser[Expr] =
      numericLit ^^ { x => new Lit(x.toInt) }
    val pAdd: PackratParser[Expr] =
      pExpr ~ ("+" ~> pExpr) ^^ { case e1 ~ e2 => new Add(e1, e2) }
    def pExpr: PackratParser[Expr] =
      pLit ||| pAdd
  }

//BEGIN_PACKRAT_RUNPARSER
val r = parse(new ExprParser {}.pExpr)("1 + 2").print // "(1 + 2)"
//END_PACKRAT_RUNPARSER

  def main(args: Array[String]): Unit = {
    println(r)
  }
}

//BEGIN_PACKRAT_PAPERCODE
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers

object Code extends StandardTokenParsers with PackratParsers {
  type Parser[E] = PackratParser[E]

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }
  // Any Scala code in the paper comes here
}
//END_PACKRAT_PAPERCODE