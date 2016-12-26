package PaperCode.Overview

import PaperCode.Overview.Util._

object Overview2 {

//BEGIN_OVERVIEW_SIMPLE_EXPR
// Abstract syntax
trait Expr { def print : String }
class Lit(x: Int) extends Expr {
  def print = x.toString
}
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + "+" + e2.print + ")"
}

// Parser with lexing
trait BaseParser {
  lexical.delimiters += ("+")
  val pLit: Parser[Expr] = numericLit ^^ { x => new Lit(x.toInt) }
  val pAdd: Parser[Expr] =
    pExpr ~ ("+" ~> pExpr) ^^{ case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: Parser[Expr] = pLit ||| pAdd
}
//END_OVERVIEW_SIMPLE_EXPR

//BEGIN_OVERVIEW_SIMPLE_LAM
class Var(x: String) extends Expr {
  def print = x
}
//END_OVERVIEW_SIMPLE_LAM

//BEGIN_OVERVIEW_SIMPLE_EXT
trait ExtParser extends BaseParser {
  val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
  val pExtExpr: Parser[Expr] = pExpr ||| pVar
}
//END_OVERVIEW_SIMPLE_EXT

  def main(args: Array[String]): Unit = {
    //val parser = new BaseParser {}.pExpr
    //runParser(parser)("a b c")

    val extParser = new ExtParser {}.pExtExpr
    parse(extParser)("a b c")
    parse(extParser)("(\\x.x) (\\y.y)")
  }
}

object Overview3 {

  abstract class Expr
  class Var(x: String) extends Expr
  class App(e1: Expr, e2: Expr) extends Expr
  class Lam(x: String, e: Expr) extends Expr

//BEGIN_OVERVIEW_OPEN_BASE
type Open[T] = (=> T) => T

trait BaseParser {
  lexical.delimiters += ("+")
  val pLit: Open[Parser[Expr]] =
    self => numericLit ^^ { x => new Lit(x.toInt) }
  val pAdd: Open[Parser[Expr]] =
    self => self ~ ("+" ~> self) ^^{ case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: Open[Parser[Expr]] =
    self => pVar(self) ||| pAdd(self)
}
//END_OVERVIEW_OPEN_BASE

/*
//BEGIN_OVERVIEW_OPEN_USE
def parse[E](p: Parser[E]): String => E = {...}
def fix[T](f: Open[T]): T = {...}
def openParse[E](p: Open[Parser[E]]): String => E = parse[E](fix(p))

openParse(new BaseParser {}.pExpr)("1 + x")
//END_OVERVIEW_OPEN_USE
*/

//BEGIN_OVERVIEW_OPEN_EXT
trait ExtParser extends BaseParser {
  lexical.delimiters += ("\\", ".")
  val pExtExpr: Open[Parser[Expr]] =
    self => pLam(self) ||| pExpr(self)
  val pLam: Open[Parser[Expr]] =
    self => "\\" ~> ident ~ ("." ~> self) ^^ { case x ~ b => new Lam(x, b) }
}
//END_OVERVIEW_OPEN_EXT

  def use2(): Unit = {
    def openParse[E](p: Open[PackratParser[E]]): String => E = parse(fix(p))
//BEGIN_OVERVIEW_OPEN_EXT_USE
openParse(new ExtParser {}.pExtExpr)("(\\x.x) (\\y.y)")
//END_OVERVIEW_OPEN_EXT_USE
  }

  def main(args: Array[String]): Unit = {
    use2()
  }
}

object Overview {

}