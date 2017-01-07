package PaperCode.Sec2Packrat


object Code1 {
//BEGIN_PACKRAT_SIMPLE_EXPR
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._

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
trait ExprParser extends StandardTokenParsers with PackratParsers {
  lexical.delimiters += "+"
  val pLit: PackratParser[Expr] =
    numericLit ^^ { x => new Lit(x.toInt) }
  val pAdd: PackratParser[Expr] =
    pExpr ~ ("+" ~> pExpr) ^^ { case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: PackratParser[Expr] =
    pLit ||| pAdd
}
//END_PACKRAT_SIMPLE_EXPR
}
