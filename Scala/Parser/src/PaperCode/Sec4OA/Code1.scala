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

  class Var(x: String) extends Expr {
    def print = x
  }

//BEGIN_FVARSEXPR
trait FVarsExpr extends Expr { def fVars: Set[String] }
//END_FVARSEXPR

//BEGIN_FVAR_CLASSES
class FVarsLit(x: Int) extends Lit(x) with FVarsExpr {
  def fVars = Set()
}

class FVarsAdd(e1: FVarsExpr, e2: FVarsExpr) extends Add(e1, e2) with FVarsExpr {
  def fVars = e1.fVars ++ e2.fVars
}

class FVarsVar(x: String) extends Var(x) with FVarsExpr {
  def fVars = Set(x)
}
//END_FVAR_CLASSES

}
