package papercode.Sec4OA


trait Code4 extends Code3 {

//BEGIN_OVERVIEW_OA_MULTI_SYNTAX
trait LamAlg[E, T] extends VarAlg[E] {
  def intT(): T
  def lam(x: String, t: T, e: E): E
}
trait LamOAParser[E, T] extends VarOAParser[E] {
  lexical.reserved += "int"
  lexical.delimiters += ("->", "\\", ":", ".")
  override val alg: LamAlg[E, T]
  val pIntT: Parser[T] = "int" ^^ { _ => alg.intT }
  val pTypedLamT: Parser[T] = pIntT
  val pLam: Parser[E] =
    ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^
      { case x ~ t ~ e => alg.lam(x, t, e) }
  val pTypedLamE: Parser[E] = pVarExpr ||| pLam
  val pT: Parser[T] = pTypedLamT
  override val pE: Parser[E] = pTypedLamE
}
//END_OVERVIEW_OA_MULTI_SYNTAX
}


object Code40 extends Code4 {
//BEGIN_OVERVIEW_OA_MULTI_SYNTAX_CLIENT
val p = new LamOAParser[String, String] {
  override val alg = new LamPrint {}
}
val r = parse(p.pE)("\\x:int. 1 + x")
//END_OVERVIEW_OA_MULTI_SYNTAX_CLIENT

  def main(args: Array[String]): Unit = {
    println(r)
  }
}
