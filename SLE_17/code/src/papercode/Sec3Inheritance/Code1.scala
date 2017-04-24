package papercode.Sec3Inheritance


object Code1 extends papercode.Sec2Packrat.Code1 {

//BEGIN_INHERITANCE_SIMPLE_LAM
class Var(x: String) extends Expr {
  def print = x
}
//END_INHERITANCE_SIMPLE_LAM

//BEGIN_INHERITANCE_BAD_ATTEMPT
trait Attempt extends AParser {
  val pVar: Parser[Expr] = ident ^^ (new Var(_))
  val pVarExpr: Parser[Expr] = pExpr ||| pVar
}
//END_INHERITANCE_BAD_ATTEMPT

//BEGIN_INHERITANCE_APPROACH
trait VarParser extends AParser {
  val pVar: Parser[Expr] = ident ^^ (new Var(_))
  override def pExpr: Parser[Expr] =
    super.pExpr ||| pVar
}
val p = new VarParser {}
val r = parse(p.pExpr)("1 + x").print // "(1+x)"
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
class BoolLit(x: Boolean) extends Expr ...
class If(p: Expr, a: Expr, b: Expr) extends Expr ...

trait BParser {
  lexical.reserved +=
    ("true", "false", "if", "then", "else")
  val pBoolLit: Parser[Expr] =
    "true" ^^^ new BoolLit(true) |||
    "false" ^^^ new BoolLit(false)
  val pIf: Parser[Expr] = ("if" ~> pExpr) ~ ("then" ~> pExpr) ~ ("else" ~> pExpr) ^^
    { case e1 ~ e2 ~ e3 => new If(e1, e2, e3) }
  def pExpr: Parser[Expr] =
    pBoolLit ||| pIf
}
trait ArithBoolParser extends AParser with BParser {
  override def pExpr: Parser[Expr] =
    super[AParser].pExpr ||| super[BParser].pExpr
}
//END_BOOL_EXAMPLE
*/

  class BoolLit(x: Boolean) extends Expr {
    def print: String = x.toString
  }
  class If(p: Expr, e1: Expr, e2: Expr) extends Expr {
    def print: String = "if " + p.print + " then " + e1.print + " else " + e2.print
  }

  trait BParser {
    lexical.reserved += ("true", "false", "if", "then", "else")
    val pBoolLit: Parser[Expr] =
      "true" ^^^ new BoolLit(true) ||| "false" ^^^ new BoolLit(false)
    val pIf: Parser[Expr] = ("if" ~> pExpr) ~ ("then" ~> pExpr) ~ ("else" ~> pExpr) ^^
      { case e1 ~ e2 ~ e3 => new If(e1, e2, e3) }
    def pExpr: Parser[Expr] =
      pBoolLit ||| pIf
  }

  trait ArithBoolParser extends AParser with BParser {
    override def pExpr: Parser[Expr] =
      super[AParser].pExpr ||| super[BParser].pExpr
  }

  def main(args: Array[String]): Unit = {
    println(r)
    val r2 = parse(new ArithBoolParser {}.pExpr)("if true then 1 + 2 else 3 + 4").print
    println(r2)
  }
}
