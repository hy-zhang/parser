package PaperCode.Sec4OA

import PaperCode.Common


object Code4 extends Common {

  trait ExprOAParser[E] {
    lexical.delimiters += "+"

    val alg: ExprAlg[E]

    val pLit: Parser[E] = numericLit ^^ { x => alg.lit(x.toInt) }
    val pAdd: Parser[E] = pE ~ ("+" ~> pE) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
    val pExpr: Parser[E] = pLit ||| pAdd

    val pE: Parser[E] = pExpr
  }

  trait VarExprOAParser[E] extends ExprOAParser[E] {
    override val alg: VarExprAlg[E]

    val pVar: Parser[E] = ident ^^ alg.varE
    val pVarExpr: Parser[E] = pExpr ||| pVar

    override val pE = pVarExpr
  }


//BEGIN_OVERVIEW_OA_MULTI_SYNTAX
trait TypedLamAlg[E, T] extends VarExprAlg[E] {
  def intT(): T
  def arrT(t1: T, t2: T): T
  def lam(x: String, t: T, e: E): E
}

trait TypedLamPrint extends TypedLamAlg[String, String] with VarExprPrint {
  def intT() = "int"
  def arrT(t1: String, t2: String) = t1 + " -> " + t2
  def lam(x: String, t: String, e: String) = "\\" + x + " : " + t + ". " + e
}

trait TypedLamOAParser[E, T] extends VarExprOAParser[E] {
  lexical.reserved += "int"
  lexical.delimiters += ("->", "\\", ":", ".")

  override val alg: TypedLamAlg[E, T]

  val pIntT: Parser[T] = "int" ^^^ alg.intT
  val pArrT: Parser[T] = pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => alg.arrT(t1, t2) }
  val pTypedLamT: Parser[T] = pIntT ||| pArrT

  val pLam: Parser[E] = ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^
    { case x ~ t ~ e => alg.lam(x, t, e) }
  val pTypedLamE: Parser[E] = pVarExpr ||| pLam

  val pT: Parser[T] = pTypedLamT
  override val pE: Parser[E] = pTypedLamE
}
//END_OVERVIEW_OA_MULTI_SYNTAX

//BEGIN_OVERVIEW_OA_MULTI_SYNTAX_CLIENT
val p = new TypedLamOAParser[String, String] {
  override val alg = new TypedLamPrint {}
}
val r = parse(p.pE)("\\x:int->int. 1 + x") // "\x : int -> int. (1 + x)"
//END_OVERVIEW_OA_MULTI_SYNTAX_CLIENT

  def main(args: Array[String]): Unit = {
    println(r)
  }
}
