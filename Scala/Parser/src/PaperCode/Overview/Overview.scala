package PaperCode.Overview

import PaperCode.Overview.Util._

object Overview2 {

//BEGIN_OVERVIEW_SIMPLE_EXPR
abstract class Expr
class Var(x: String) extends Expr
class App(e1: Expr, e2: Expr) extends Expr

trait BaseParser {
  lexical.delimiters += ("(", ")")
  val pExpr: PackratParser[Expr] = pVar ||| pApp ||| "(" ~> pExpr <~ ")"
  val pVar: PackratParser[Expr] = ident ^^ { x => new Var(x) }
  val pApp: PackratParser[Expr] =
    pExpr ~ pExpr ^^ { case e1 ~ e2 => new App(e1, e2) }
}
//END_OVERVIEW_SIMPLE_EXPR

//BEGIN_OVERVIEW_SIMPLE_LAM
class Lam(x: String, e: Expr) extends Expr
//END_OVERVIEW_SIMPLE_LAM

//BEGIN_OVERVIEW_SIMPLE_EXT
trait ExtParser extends BaseParser {
  lexical.delimiters += ("\\", ".")
  val pExtExpr: PackratParser[Expr] =
    pExpr ||| pLam ||| "(" ~> pExtExpr <~ ")"
  val pLam: PackratParser[Expr] =
    "\\" ~> ident ~ ("." ~> pExtExpr) ^^ { case x ~ e => new Lam(x, e) }
}
//END_OVERVIEW_SIMPLE_EXT

  def main(args: Array[String]): Unit = {
    //val parser = new BaseParser {}.pExpr
    //runParser(parser)("a b c")

    val extParser = new ExtParser {}.pExtExpr
    runParser(extParser)("a b c")
    runParser(extParser)("(\\x.x) (\\y.y)")
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
  val pVar: Open[PackratParser[Expr]] =
    self => ident ^^ { x => new Var(x) }
  val pApp: Open[PackratParser[Expr]] =
    self => self ~ self ^^ { case e1 ~ e2 => new App(e1, e2) }
  val pExpr: Open[PackratParser[Expr]] =
    self => pVar(self) ||| pApp(self) ||| "(" ~> self <~ ")"
}
//END_OVERVIEW_OPEN_BASE

  def use(): Unit = {
//BEGIN_OVERVIEW_OPEN_USE
def fix[T](f: Open[T]): T = { lazy val a: T = f(a); a }

def parse[E](p: Open[PackratParser[E]])(inp: String): E = {
  val t = phrase(fix(p))(new lexical.Scanner(inp))
  if (t.successful) t.get else scala.sys.error(t.toString)
}

println(parse(new BaseParser {}.pExpr)("x y"))
//END_OVERVIEW_OPEN_USE
  }

//BEGIN_OVERVIEW_OPEN_EXT
trait ExtParser extends BaseParser {
  lexical.delimiters += ("\\", ".")
  val pLam: Open[PackratParser[Expr]] =
    self => "\\" ~> ident ~ ("." ~> self) ^^ { case x ~ b => new Lam(x, b) }
  val pExtExpr: Open[PackratParser[Expr]] =
    self => pLam(self) ||| pExpr(self)
}
//END_OVERVIEW_OPEN_EXT

  def use2(): Unit = {
    def parse[E](p: Open[PackratParser[E]])(inp: String): E = {
      val t = phrase(fix(p))(new lexical.Scanner(inp))
      if (t.successful) t.get else scala.sys.error(t.toString)
    }
//BEGIN_OVERVIEW_OPEN_EXT_USE
println(parse(new ExtParser {}.pExtExpr)("(\\x.x) (\\y.y)"))
//END_OVERVIEW_OPEN_EXT_USE
  }
}

object Overview {

}