package PaperCode.Overview

import PaperCode.Overview.Util._

object Overview2 {

//BEGIN_OVERVIEW_SIMPLE_EXPR
abstract class Expr
class Var(x: String) extends Expr
class App(e1: Expr, e2: Expr) extends Expr

trait BaseParser {
  lexical.delimiters += ("(", ")")
  val pExpr: Parser[Expr] = pVar ||| pApp ||| pParen
  val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
  val pApp: Parser[Expr] = pExpr ~ pExpr ^^ { case e1 ~ e2 => new App(e1, e2) }
  val pParen: Parser[Expr] = "(" ~> pExpr <~ ")"
}
//END_OVERVIEW_SIMPLE_EXPR

//BEGIN_OVERVIEW_SIMPLE_LAM
class Lam(x: String, e: Expr) extends Expr
//END_OVERVIEW_SIMPLE_LAM

//BEGIN_OVERVIEW_SIMPLE_EXT
trait ExtParser extends BaseParser {
  lexical.delimiters += ("\\", ".")
  val pExtExpr: Parser[Expr] = pExpr ||| pLam
  val pLam: Parser[Expr] =
    "\\" ~> ident ~ ("." ~> pExtExpr) ^^ { case x ~ e => new Lam(x, e) }
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
  lexical.delimiters += ("(", ")")
  val pVar: Open[Parser[Expr]] =
    self => ident ^^ { x => new Var(x) }
  val pApp: Open[Parser[Expr]] =
    self => self ~ self ^^ { case e1 ~ e2 => new App(e1, e2) }
  val pExpr: Open[Parser[Expr]] =
    self => pVar(self) ||| pApp(self) ||| "(" ~> self <~ ")"
}
//END_OVERVIEW_OPEN_BASE

/*
//BEGIN_OVERVIEW_OPEN_USE
def parse[E](p: Parser[E]): String => E = {...}
def fix[T](f: Open[T]): T = {...}
def openParse[E](p: Open[Parser[E]]): String => E = parse[E](fix(p))

openParse(new BaseParser {}.pExpr)("x y")
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