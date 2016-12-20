package PaperCode.Overview

import PaperCode.Overview.Util._

object OA {

//BEGIN_OVERVIEW_OA_ALG
trait ExprAlg[E] {
  def Var(x: String): E
  def App(e1: E, e2: E): E
}
//END_OVERVIEW_OA_ALG

//BEGIN_OVERVIEW_OA_PRINT
trait ExprPrint extends ExprAlg[String] {
  def Var(x: String): String = x
  def App(e1: String, e2: String): String = "(" + e1 + " " + e2 + ")"
}
//END_OVERVIEW_OA_PRINT

//BEGIN_OVERVIEW_OA_PARSER
trait ExprParser[E] {
  lexical.delimiters += ("(", ")")
  val pExpr: ExprAlg[E] => Open[PackratParser[E]] = alg => self =>
    ident ^^ alg.Var |||
      self ~ self ^^ { case e1 ~ e2 => alg.App(e1, e2) } |||
      "(" ~> self <~ ")"
}
//END_OVERVIEW_OA_PARSER

//BEGIN_OVERVIEW_OA_EXT
trait LambdaAlg[E] extends ExprAlg[E] {
  def Lam(x: String, e: E): E
}

trait LambdaPrint extends LambdaAlg[String] with ExprPrint {
  def Lam(x: String, e: String): String = "\\" + x + "." + e
}

trait LambdaParser[E] extends ExprParser[E] {
  lexical.delimiters += ("\\", ".")
  val pLam: LambdaAlg[E] => Open[PackratParser[E]] = alg => self =>
    ("\\" ~> ident) ~ ("." ~> self) ^^ { case x ~ e => alg.Lam(x, e) }
  val pExtExpr: LambdaAlg[E] => Open[PackratParser[E]] =
    pExpr | pLam
}
//END_OVERVIEW_OA_EXT

  def openParse[E](p: Open[PackratParser[E]])(inp: String): E = {
    val t = phrase(fix(p))(new lexical.Scanner(inp))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

//BEGIN_OVERVIEW_OA_USE
def parseLambdaAlg[E](inp: String)(alg: LambdaAlg[E]): E = {
  val p = new LambdaParser[E] {}.pExtExpr(alg)
  openParse[E](p)(inp)
}

parseLambdaAlg("(\\x.x) (\\y.y)")(new LambdaPrint{})
//END_OVERVIEW_OA_USE


//BEGIN_OVERVIEW_OA_MULTI_SYNTAX
trait TypedLambdaAlg[E, T] extends ExprAlg[E] {
  def Lam(x: String, t: T, e: E): E
  def IntType(): T
  def ArrowType(a: T, b: T): T
}
//END_OVERVIEW_OA_MULTI_SYNTAX


//BEGIN_OVERVIEW_OA_EXT_OP
trait CollectFreeVars extends LambdaAlg[Set[String]] {
  def Var(x: String): Set[String] = Set(x)
  def App(e1: Set[String], e2: Set[String]): Set[String] = e1 ++ e2
  def Lam(x: String, e: Set[String]): Set[String] = e - x
}

parseLambdaAlg("\\x.(x y)")(new CollectFreeVars {})
//END_OVERVIEW_OA_EXT_OP


  def main(args: Array[String]): Unit = {

  }

}
