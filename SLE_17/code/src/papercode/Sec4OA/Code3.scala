package papercode.Sec4OA

import papercode.Common


trait Code3 extends Common {
//BEGIN_BASE_OA_PARSER_BAD
trait Attempt[E] {
  lexical.delimiters += "+"
  val pLit: Alg[E] => Parser[E] = alg =>
    numericLit ^^ { x => alg.lit(x.toInt) }
  val pAdd: Alg[E] => Parser[E] = alg =>
    pExpr(alg) ~ ("+" ~> pExpr(alg)) ^^
      { case e1 ~ e2 => alg.add(e1, e2) }
  val pExpr: Alg[E] => Parser[E] = alg =>
    pLit(alg) ||| pAdd(alg)
}
//END_BASE_OA_PARSER_BAD

//BEGIN_BASE_OA_PARSER
trait OAParser[E] {
  lexical.delimiters += "+"
  val alg: Alg[E]
  val pLit: Parser[E] = numericLit ^^
    { x => alg.lit(x.toInt) }
  val pAdd: Parser[E] = pE ~ ("+" ~> pE) ^^
    { case e1 ~ e2 => alg.add(e1, e2) }
  val pExpr: Parser[E] = pLit ||| pAdd
  val pE: Parser[E] = pExpr
}
//END_BASE_OA_PARSER

//BEGIN_EXT_OA_PARSER
trait VarOAParser[E] extends OAParser[E] {
  override val alg: VarAlg[E]
  val pVar: Parser[E] = ident ^^ alg.varE
  val pVarExpr: Parser[E] = pExpr ||| pVar
  override val pE: Parser[E] = pVarExpr
}
//END_EXT_OA_PARSER
}


object Code30 extends Code3 {
//BEGIN_EXT_OA_PARSER_CLIENT
val p = new VarOAParser[String] {
  override val alg = new VarPrint {}
}
val r = parse(p.pE)("1 + x") // "(1 + x)"
//END_EXT_OA_PARSER_CLIENT

  def main(args: Array[String]): Unit = {
    println(r)
  }
}