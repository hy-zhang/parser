package PaperCode.Sec4OA


object Code1 {

  trait Expr {
    def print: String
  }

  class Lit(x: Int) extends Expr {
    def print = x.toString
  }

  class Add(e1: Expr, e2: Expr) extends Expr {
    def print = "(" + e1.print + " + " + e2.print + ")"
  }

//BEGIN_ATTEMPT_EXPRWITHFREEVARS
trait FreeVarsExpr extends Expr { def freeVars: Set[String] }
//END_ATTEMPT_EXPRWITHFREEVARS

}
