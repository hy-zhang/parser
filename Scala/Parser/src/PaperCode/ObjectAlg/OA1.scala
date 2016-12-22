package PaperCode.ObjectAlg

//BEGIN_OBJECT_ALG_EXPR
trait ExprAlg[E] {
  def Var(x: String): E
  def App(e1: E, e2: E): E
}

//END_OBJECT_ALG_EXPR

//BEGIN_OBJECT_ALG_FREE_VARS
trait FreeVars extends ExprAlg[Set[String]] {
  def Var(x: String) = Set(x)
  def App(e1: Set[String], e2: Set[String]): Set[String] = e1 ++ e2
}
//END_OBJECT_ALG_FREE_VARS

//BEGIN_OBJECT_ALG_EXPR_PRINT
trait ExprPrint extends ExprAlg[String] {
  def Var(x: String): String = x
  def App(e1: String, e2: String): String = "(" + e1 + " " + e2 + ")"
}
//END_OBJECT_ALG_EXPR_PRINT

//BEGIN_OBJECT_ALG_LIT
trait LitAlg[E] extends ExprAlg[E] {
  def Lit(n: Int): E
}
//END_OBJECT_ALG_LIT

//BEGIN_OBJECT_ALG_LIT_PRINT
trait LitPrint extends ExprPrint {
  def Lit(n: Int): String = n.toString
}
//END_OBJECT_ALG_LIT_PRINT

//BEGIN_OBJECT_ALG_REFACTOR
trait Refactor[E] extends ExprAlg[E] {
  val alg: LitAlg[E]
  val env: Map[String, Int]
  def Var(x: String): E = if (env.contains(x)) alg.Lit(env(x)) else alg.Var(x)
}
//END_OBJECT_ALG_REFACTOR

object OA1 {

//BEGIN_OBJECT_ALG_BUILD
def build[E](alg: LitAlg[E]): E = alg.App(alg.Var("x"), alg.Lit(3))
//END_OBJECT_ALG_BUILD

}
