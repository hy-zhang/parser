package papercode.Sec4OA


//BEGIN_OVERVIEW_OA_ALG
trait Alg[E] {
  def lit(n: Int): E
  def add(e1: E, e2: E): E }
//END_OVERVIEW_OA_ALG

//BEGIN_OVERVIEW_OA_PRINT
trait Print extends Alg[String] {
  def lit(n: Int) = n.toString
  def add(e1: String, e2: String) =
    "(" + e1 + " + " + e2 + ")" }
//END_OVERVIEW_OA_PRINT

//BEGIN_OVERVIEW_OA_EVAL
trait Eval extends Alg[Int] {
  def lit(n: Int) = n
  def add(e1: Int, e2: Int) = e1 + e2 }
//END_OVERVIEW_OA_EVAL

//BEGIN_OVERVIEW_OA_ALGEXT
trait VarAlg[E] extends Alg[E] {
  def varE(x: String): E }
//END_OVERVIEW_OA_ALGEXT

//BEGIN_OVERVIEW_OA_EXTPRINT
trait VarPrint extends VarAlg[String] with Print {
  def varE(x: String) = x }
//END_OVERVIEW_OA_EXTPRINT


object Code2 {

//BEGIN_OVERVIEW_OA_MAKEEXP
def makeExp[E](alg: VarAlg[E]): E =
  alg.add(alg.lit(1), alg.varE("x"))
//END_OVERVIEW_OA_MAKEEXP

//BEGIN_OVERVIEW_OA_REFACTOR
trait Refactor[E] extends VarAlg[E] {
  val alg: Alg[E]
  val env: Map[String, Int]
  def lit(n: Int): E = alg.lit(n)
  def add(e1: E, e2: E): E = alg.add(e1, e2)
  def varE(x: String): E = alg.lit(env(x))
}

val r = makeExp(new Refactor[String] {
  override val alg = new Print {}
  override val env = Map("x" -> 2)
}) // "(1 + 2)"
//END_OVERVIEW_OA_REFACTOR

  def main(args: Array[String]): Unit = {
    println(makeExp(new VarPrint {}))
  }
}
