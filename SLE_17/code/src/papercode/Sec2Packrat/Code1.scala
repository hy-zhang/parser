package papercode.Sec2Packrat

import papercode.Common


trait Code1 extends Common {

//BEGIN_PACKRAT_OO_AST
trait Expr { def print: String }
class Lit(x: Int) extends Expr {
  def print = x.toString }
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + "+" + e2.print + ")" }
//END_PACKRAT_OO_AST

//BEGIN_PACKRAT_SIMPLE_EXPR
trait AParser {
  lexical.delimiters += "+"
  val pLit: Parser[Expr] = numericLit ^^
    { x => new Lit(x.toInt) }
  val pAdd: Parser[Expr] = pExpr ~ ("+" ~> pExpr) ^^
    { case e1 ~ e2 => new Add(e1, e2) }
  def pExpr: Parser[Expr] = pLit ||| pAdd
}
//END_PACKRAT_SIMPLE_EXPR
}
