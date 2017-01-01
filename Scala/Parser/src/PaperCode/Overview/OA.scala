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

//BEGIN_OVERVIEW_OA_ALGEXT
trait VarExprAlg[E] extends ExprAlg[E] {
  def varE(x: String): E
}
//END_OVERVIEW_OA_ALGEXT

//BEGIN_OVERVIEW_OA_EXTPRINT
trait VarExprPrint extends VarExprAlg[String] with Print {
  def varE(x: String) = x
}
//END_OVERVIEW_OA_EXTPRINT

//BEGIN_OVERVIEW_OA_MAKEEXP
def makeExp[E](alg: VarExprAlg[E]): E = alg.add(alg.lit(1), alg.varE("x"))
//END_OVERVIEW_OA_MAKEEXP

//BEGIN_OVERVIEW_OA_REFACTOR
trait Refactor[E] extends VarExprAlg[E] {
  val alg: ExprAlg[E]
  val env: Map[String, Int]
  def lit(n: Int) = alg.lit(n)
  def add(e1: E, e2: E) = alg.add(e1, e2)
  def varE(x: String) = alg.lit(env(x))
}

makeExp(new Refactor {
  override val alg = new Print {}
  override val env = Map("x" -> 2)
}) // "(1 + 2)"
//END_OVERVIEW_OA_REFACTOR

//BEGIN_BASE_OA_PARSER_BAD
trait ExprOAParser[E] {
  lexical.delimiters += ("+")
  val pLit: ExprAlg[E] => Parser[E] = alg => numericLit ^^ { x => alg.lit(x.toInt) }
  val pAdd: ExprAlg[E] => Parser[E] = alg => {
    val p = pExpr(alg)
    p ~ ("+" ~> p) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
  }
  val pExpr: ExprAlg[E] => Parser[E] = alg => pLit(alg) ||| pAdd(alg)
}
//END_BASE_OA_PARSER_BAD

//BEGIN_BASE_OA_PARSER
trait ExprOAParser[E] {
  lexical.delimiters += ("+")
  val alg: ExprAlg[E]
  
  val pLit: Parser[E] = numericLit ^^ { x => alg.lit(x.toInt) }
  val pAdd: Parser[E] = pE ~ ("+" ~> pE) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
  val pExprE: Parser[E] = pLit ||| pAdd
  
  val pE: Parser[E] = pLitAdd
}
//END_BASE_OA_PARSER

//BEGIN_EXT_OA_PARSER
trait VarExprOAParser[E] extends ExprOAParser[E] {
  override val alg: VarExprAlg[E]
  
  val pVar: Parser[E] = ident ^^ alg.varE
  val pVarExprE: Parser[E] = pExprE ||| pVar
  
  override val pE = pVarExprE
}

val result = parse(new VarExprOAParser[String] {
  override val alg = new VarExprPrint {}
}.pE)("1 + x") // "(1 + x)"
//END_EXT_OA_PARSER

//BEGIN_OVERVIEW_OA_MULTI_SYNTAX
trait TypedLamAlg[E, T] extends VarExprAlg[E] {
  def intT(): T
  def arrowT(t1: T, t2: T): T
  def lam(x: String, t: T, e: E): E
}

trait TypedLamPrint extends TypedLamAlg[String, String] with VarExprPrint {
  def intT() = "int"
  def arrowT(t1: String, t2: String) = t1 + " -> " + t2
  def lam(x: String, t: String, e: String) = "\\" + x + " : " + t + ". " + e
}

trait TypedLamOAParser[E, T] extends VarExprOAParser[E] {
  lexical.reserved += ("int")
  lexical.delimiters += ("->", "\\", ":", ".")
  override val alg: TypedLamAlg[E, T]
  
  val pIntT: Parser[T] = "int" ^^^ alg.intT
  val pArrowT: Parser[T] = pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => alg.arrowT(t1, t2) }
  val pTypedLamT: Parser[T] = pIntT ||| pArrowT
  
  val pLam: Parser[E] = ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^
    { case x ~ t ~ e => alg.lam(x, t, e) }
  val pTypedLamE: Parser[E] = pVarExprE ||| pLam
  
  val pT: Parser[T] = pTypedLamT
  override val pE = pTypedLamE
}

//END_OVERVIEW_OA_MULTI_SYNTAX

//BEGIN_OVERVIEW_OA_MULTI_SYNTAX_CLIENT
val result2 = parse(new TypedLamOAParser[String, String] {
  override val alg = new TypedLamPrint {}
}.pE)("\\x:int->int. 1 + x") // "\x : int -> int. (1 + x)"
//END_OVERVIEW_OA_MULTI_SYNTAX_CLIENT

  def main(args: Array[String]): Unit = {

  }

}
