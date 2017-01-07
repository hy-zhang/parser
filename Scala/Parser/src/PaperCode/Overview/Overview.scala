package PaperCode.Overview


object Overview {

trait Expr { def print : String }
class Lit(x: Int) extends Expr {
  def print = x.toString
}
class Add(e1: Expr, e2: Expr) extends Expr {
  def print = "(" + e1.print + " + " + e2.print + ")"
}

//BEGIN_OVERVIEW_ATTEMPT_EXPRWITHEVAL
trait EvalExpr extends Expr { def eval: Int }
//END_OVERVIEW_ATTEMPT_EXPRWITHEVAL
class EvalLit(x: Int) extends Lit(x) with EvalExpr {
  def eval = x
}
class EvalAdd(e1: EvalExpr, e2: EvalExpr) extends Add(e1, e2) with EvalExpr {
  def eval = e1.eval + e2.eval
}

}