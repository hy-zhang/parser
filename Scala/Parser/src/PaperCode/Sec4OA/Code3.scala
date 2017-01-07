package PaperCode.Sec4OA

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._


object Code30 extends StandardTokenParsers with PackratParsers {
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


object Code3 extends StandardTokenParsers with PackratParsers {
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

  override val pE = pVarExpr
}

val r = parse(new VarExprOAParser[String] {
  override val alg = new VarExprPrint {}
}.pE)("1 + x") // "(1 + x)"
//END_EXT_OA_PARSER

  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    if (t.successful) t.get else scala.sys.error(t.toString)
  }

  def main(args: Array[String]): Unit = {
    println(r)
  }
}