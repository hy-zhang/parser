package PaperCode.Overview

import PaperCode.Overview.Util._

/*
//BEGIN_OVERVIEW_SIMPLE_EXPR
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers

// Abstract syntax
trait Expr { def print: String }
class Lit(x: Int) extends Expr {
  def print = x.toString
}
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + " + " + e2.print + ")"
}

// Parser with lexing
trait BaseParser {
  lexical.delimiters += ("+")
  val pLit: PackratParser[Expr] =
    numericLit ^^ { x => new Lit(x.toInt) }
  val pAdd: PackratParser[Expr] =
    pExpr ~ ("+" ~> pExpr) ^^ { case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: PackratParser[Expr] = pLit ||| pAdd
}
//END_OVERVIEW_SIMPLE_EXPR
*/

object Latest {

type Parser[E] = PackratParser[E]

trait Expr { def print: String }
class Lit(x: Int) extends Expr {
  def print = x.toString
}
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + " + " + e2.print + ")"
}

trait BaseParser {
  lexical.delimiters += ("+")
  val pLit: Parser[Expr] = numericLit ^^ { x => new Lit(x.toInt) }
  val pAdd: Parser[Expr] =
    pExpr ~ ("+" ~> pExpr) ^^ { case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: Parser[Expr] = pLit ||| pAdd
}

//BEGIN_PACKRAT_RUNPARSER
def parse[E](p: Parser[E]): String => E = in => {
  val t = phrase(p)(new lexical.Scanner(in))
  if (t.successful) t.get else scala.sys.error(t.toString)
}

val result: String = parse(new BaseParser {}.pExpr)("1 + 2").print // "(1 + 2)"
//END_PACKRAT_RUNPARSER  

/*
//BEGIN_BAD_ATTEMPT
val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
val pExtExpr = pExpr ||| pVar
//END_BAD_ATTEMPT
*/

//BEGIN_INHERITANCE_APPROACH
trait ExtParser extends BaseParser {
  val pVar: Parser[Expr] = ident ^^ { x => new Var(x) }
  override val pExpr = super.pExpr ||| pVar
}

val result2: String = parse(new ExtParser {}.pExpr)("1 + x").print // "(1 + x)"
//END_INHERITANCE_APPROACH

/*
//BEGIN_MULTIPLE_INHERITANCE
trait LanguageA {...}

trait LanguageB {...}

trait LanguageC extends LanguageA with LanguageB {
  override val pExpr = super[LanguageA].pExpr ||| super[LanguageB].pExpr
}
//END_MULTIPLE_INHERITANCE
*/

//BEGIN_ATTEMPT_EXPRWITHFREEVARS
trait FreeVarsExpr extends Expr { def freeVars: Set[String] }
//END_ATTEMPT_EXPRWITHFREEVARS
}

object Overview2 {



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
    self => self ~ ("+" ~> self) ^^ { case e1 ~ e2 => new Add(e1, e2) }
  val pExpr: Open[Parser[Expr]] =
    self => pVar(self) ||| pAdd(self)
}
//END_OVERVIEW_OPEN_BASE

/*
//BEGIN_OVERVIEW_OPEN_USE
def parse[E](p: Parser[E]): String => E = {...}
def fix[T](f: Open[T]): T = {...}
def openParse[E](p: Open[Parser[E]]): String => E = parse[E](fix(p))

openParse(new BaseParser {}.pExpr)("1 + 2").print // "(1 + 2)"
//END_OVERVIEW_OPEN_USE
*/

//BEGIN_OVERVIEW_OPEN_EXT
trait ExtParser extends BaseParser {
  val pVar: Open[Parser[Expr]] = self => ident ^^ { x => new Var(x) }
  val pExtExpr: Open[Parser[Expr]] = self => pExpr(self) ||| pVar(self)
}
//END_OVERVIEW_OPEN_EXT

  def use2(): Unit = {
    def openParse[E](p: Open[PackratParser[E]]): String => E = parse(fix(p))
//BEGIN_OVERVIEW_OPEN_EXT_USE
openParse(new ExtParser {}.pExtExpr)("1 + x").print // "(1 + x)"
//END_OVERVIEW_OPEN_EXT_USE
  }

  def main(args: Array[String]): Unit = {
    use2()
  }
}

object Overview {

trait Expr { def print : String }
class Lit(x: Int) extends Expr {
  def print = x.toString
}
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + " + " + e2.print + ")"
}

//BEGIN_OVERVIEW_ATTEMPT_EXPRWITHEVAL
trait EvalExpr extends Expr { def eval: Int }
//END_OVERVIEW_ATTEMPT_EXPRWITHEVAL
class EvalLit(x: Int) extends Lit(x) with EvalExpr {
  def eval = x
}
class EvalAdd(e1: EvalExpr, e2: EvalExpr) extends Add(e1, e2) with EvalExpr {
  def eval = e1.eval + e2.eval
}

}