package PaperCode.Sec4OA

import PaperCode.Common


object Code30 extends Common {
//BEGIN_BASE_OA_PARSER_BAD
trait ExprOAParser[E] {
  lexical.delimiters += "+"
  val pLit: ExprAlg[E] => Parser[E] = alg => numericLit ^^ { x => alg.lit(x.toInt) }
  val pAdd: ExprAlg[E] => Parser[E] = alg => {
    val p = pExpr(alg)
    p ~ ("+" ~> p) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
  }
  val pExpr: ExprAlg[E] => Parser[E] = alg => pLit(alg) ||| pAdd(alg)
}
//END_BASE_OA_PARSER_BAD
}


object Code3 extends Common {
//BEGIN_BASE_OA_PARSER
trait ExprOAParser[E] {
  lexical.delimiters += "+"
  val alg: ExprAlg[E]
  val pLit: Parser[E] = numericLit ^^ { x => alg.lit(x.toInt) }
  val pAdd: Parser[E] = pE ~ ("+" ~> pE) ^^ { case e1 ~ e2 => alg.add(e1, e2) }
  val pExpr: Parser[E] = pLit ||| pAdd
  val pE: Parser[E] = pExpr
}
//END_BASE_OA_PARSER

//BEGIN_EXT_OA_PARSER
trait VarExprOAParser[E] extends ExprOAParser[E] {
  override val alg: VarExprAlg[E]
  val pVar: Parser[E] = ident ^^ alg.varE
  val pVarExpr: Parser[E] = pExpr ||| pVar
  override val pE: Parser[E] = pVarExpr
}
//END_EXT_OA_PARSER

//BEGIN_EXT_OA_PARSER_CLIENT
val p = new VarExprOAParser[String] {
  override val alg = new VarExprPrint {}
}
val r = parse(p.pE)("1 + x") // "(1 + x)"
//END_EXT_OA_PARSER_CLIENT

  def main(args: Array[String]): Unit = {
    println(r)
  }
}