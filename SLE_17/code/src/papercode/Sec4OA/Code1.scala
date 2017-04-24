package papercode.Sec4OA


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
trait NewExpr extends Expr { def free: Set[String] }
//END_FVARSEXPR

}
