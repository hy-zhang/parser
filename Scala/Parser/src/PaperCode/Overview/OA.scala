package PaperCode.Overview

import PaperCode.Overview.Util._

object OA {

//BEGIN_OVERVIEW_OA_ALG
trait ExprAlg[E] {
  def lit(n: Int): E
  def add(e1: E, e2: E): E
}
//END_OVERVIEW_OA_ALG

//BEGIN_OVERVIEW_OA_PRINT
trait Print extends ExprAlg[String] {
  def lit(n: Int) = x.toString
  def add(e1: String, e2: String) = "(" + e1 + " + " + e2 + ")"
}
//END_OVERVIEW_OA_PRINT

//BEGIN_OVERVIEW_OA_EVAL
trait Eval extends ExprAlg[Int] {
  def lit(n: Int) = n
  def add(e1: Int, e2: Int) = e1 + e2
}
//END_OVERVIEW_OA_EVAL

//BEGIN_OVERVIEW_OA_PARSER
trait BaseParser[E] {
  lexical.delimiters += ("+")
  val pExpr: ExprAlg[E] => Open[Parser[E]] = alg => self =>
    numericLit ^^ alg.lit |||
    self => self ~ ("+" ~> self) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
}
//END_OVERVIEW_OA_PARSER

//BEGIN_OVERVIEW_OA_EXT
trait VarAlg[E] {
  def varE(x: String): E
}

trait PrintVar extends VarAlg[String] {
  def varE(x: String) = x
}

trait ExtParser[E] extends BaseParser[E] {
  val pVar: VarAlg[E] => Open[Parser[E]] = alg => self =>
    ident ^^ alg.varE
  val pExtExpr: ExprAlg[E] with VarAlg[E] => Open[Parser[E]] =
    pExpr <|> pVar
}
//END_OVERVIEW_OA_EXT

  def openParse[E](p: Open[PackratParser[E]])(inp: String): E = {
    val t = phrase(fix(p))(new lexical.Scanner(inp))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

//BEGIN_OVERVIEW_OA_USE
def parseExprVarAlg[E](inp: String)(alg: ExprAlg[E] with VarAlg[E}): E = {
  val p = new ExtParser[E] {}.pExtExpr(alg)
  openParse[E](p)(inp)
}

parseExprVarAlg("1 + x")(new Print with PrintVar{}) // "(1 + x)"
//END_OVERVIEW_OA_USE


//BEGIN_OVERVIEW_OA_MULTI_SYNTAX
trait TypedLambdaAlg[E, T] {
  def intType(): T
  def arrowType(a: T, b: T): T
  def lam(x: String, t: T, e: E): E
}
//END_OVERVIEW_OA_MULTI_SYNTAX


//BEGIN_OVERVIEW_OA_EXT_OP
trait FreeVars extends ExprAlg[Set[String]] with VarAlg[Set[String]] {
  def lit(x: Int) = Set()
  def add(e1: Set[String], e2: Set[String]) = e1 ++ e2
  def varE(x: String) = Set(x)
}

parseExprVarAlg("1 + x")(new FreeVars {}) // Set(x)
//END_OVERVIEW_OA_EXT_OP


  def main(args: Array[String]): Unit = {

  }

}
