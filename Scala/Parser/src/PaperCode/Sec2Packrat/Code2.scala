package PaperCode.Sec2Packrat

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._


object Code2 extends StandardTokenParsers with PackratParsers {

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
    val pExpr: PackratParser[Expr] =
      pLit ||| pAdd
  }

  def main(args: Array[String]): Unit = {

//BEGIN_PACKRAT_RUNPARSER
type Parser[E] = PackratParser[E]

def parse[E](p: Parser[E]): String => E = in => {
  val t = phrase(p)(new lexical.Scanner(in))
  if (t.successful) t.get else scala.sys.error(t.toString)
}

parse(new ExprParser {}.pExpr)("1 + 2").print // "(1 + 2)"
//END_PACKRAT_RUNPARSER

    println(parse(new ExprParser {}.pExpr)("1 + 2").print)
  }
}
