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


  // ---------------------------------------------------

  trait LanguageA {
    def pExpr: Parser[String] = ident
  }

  trait LanguageB {
    def pExpr: Parser[String] = ident
  }

  trait LanguageC extends LanguageA with LanguageB {
    override def pExpr = super[LanguageA].pExpr ||| super[LanguageB].pExpr
  }

/*
//BEGIN_MULTIPLE_INHERITANCE
trait LanguageA {...}

trait LanguageB {...}

trait LanguageC extends LanguageA with LanguageB {
  override def pExpr = super[LanguageA].pExpr ||| super[LanguageB].pExpr
}
//END_MULTIPLE_INHERITANCE
*/

/*
//BEGIN_BOOL_EXAMPLE
class BoolLit(x: Boolean) extends Expr {...}
class If(p: Expr, e1: Expr, e2: Expr) extends Expr {...}

trait BoolParser {
  lexical.reserved += ("true", "false", "if", "then", "else")
  val pBoolLit: Parser[Expr] =
    "true" ^^^ new BoolLit(true) ||| "false" ^^^ new BoolLit(false)
  val pIf: Parser[Expr] = ("if" ~> pExpr) ~ ("then" ~> pExpr) ~ ("else" ~> pExpr) ^^
    { case e1 ~ e2 ~ e3 => new If(e1, e2, e3) }
  def pExpr: Parser[Expr] =
    pBoolLit ||| pIf
}

trait ArithBoolParser extends ExprParser with BoolParser {
  override def pExpr: Parser[Expr] =
    super[ExprParser].pExpr ||| super[BoolParser].pExpr
}
//END_BOOL_EXAMPLE
*/

  class BoolLit(x: Boolean) extends Expr {
    def print: String = x.toString
  }
  class If(p: Expr, e1: Expr, e2: Expr) extends Expr {
    def print: String = "if " + p.print + " then " + e1.print + " else " + e2.print
  }

  trait BoolParser {
    lexical.reserved += ("true", "false", "if", "then", "else")
    val pBoolLit: Parser[Expr] =
      "true" ^^^ new BoolLit(true) ||| "false" ^^^ new BoolLit(false)
    val pIf: Parser[Expr] = ("if" ~> pExpr) ~ ("then" ~> pExpr) ~ ("else" ~> pExpr) ^^
      { case e1 ~ e2 ~ e3 => new If(e1, e2, e3) }
    def pExpr: Parser[Expr] =
      pBoolLit ||| pIf
  }

  trait ArithBoolParser extends ExprParser with BoolParser {
    override def pExpr: Parser[Expr] =
      super[ExprParser].pExpr ||| super[BoolParser].pExpr
  }

  def main(args: Array[String]): Unit = {
    println(r)
    val r2 = parse(new ArithBoolParser {}.pExpr)("if true then 1 + 2 else 3 + 4").print
    println(r2)
  }
}
