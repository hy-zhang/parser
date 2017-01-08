package PaperCode.Sec3Inheritance

import PaperCode.Common


object Code1 extends Common {

  trait Expr {
    def print: String
  }
  class Lit(x: Int) extends Expr {
    def print = x.toString
  }
  class Add(e1: Expr, e2: Expr) extends Expr {
    def print = "(" + e1.print + " + " + e2.print + ")"
  }

  trait ExprParser {
    lexical.delimiters += "+"
    val pLit: PackratParser[Expr] =
      numericLit ^^ { x => new Lit(x.toInt) }
    val pAdd: PackratParser[Expr] =
      pExpr ~ ("+" ~> pExpr) ^^ { case e1 ~ e2 => new Add(e1, e2) }
    def pExpr: PackratParser[Expr] =
      pLit ||| pAdd
  }

//BEGIN_INHERITANCE_SIMPLE_LAM
class Var(x: String) extends Expr {
  def print = x
}
//END_INHERITANCE_SIMPLE_LAM

//BEGIN_INHERITANCE_BAD_ATTEMPT
trait Attempt extends ExprParser {
  val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
  val pVarExpr: Parser[Expr] = pExpr ||| pVar
}
//END_INHERITANCE_BAD_ATTEMPT

//BEGIN_INHERITANCE_APPROACH
trait VarExprParser extends ExprParser {
  val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
  override def pExpr: Parser[Expr] = super.pExpr ||| pVar
}

val r = parse(new VarExprParser {}.pExpr)("1 + x").print // "(1 + x)"
//END_INHERITANCE_APPROACH

  def main(args: Array[String]): Unit = {
    println(r)
  }

/*
//BEGIN_MULTIPLE_INHERITANCE
trait LanguageA {...}

trait LanguageB {...}

trait LanguageC extends LanguageA with LanguageB {
  override val pExpr = super[LanguageA].pExpr ||| super[LanguageB].pExpr
}
//END_MULTIPLE_INHERITANCE
*/
}
