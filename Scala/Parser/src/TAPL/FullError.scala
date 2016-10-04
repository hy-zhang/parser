package TAPL

/* <8> */
object FullError {
  import Util._
  trait FullErrorAlg[E] {
    def TmError(): E
    def TmTry(e1: E, e2: E): E
  }
  trait Lexer extends FullErrorAlg[String] {
    lexical.reserved += ("error", "try", "with")
    def TmError() = "error"
    def TmTry(e1: String, e2: String) = "try " + e1 + " with " + e2
  }
  trait Parser[E, F <: {val pE: PackratParser[E]}] {
    lazy val pE: FullErrorAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      List(
        "error" ^^ { _ => alg.TmError() },
        "try" ~> e ~ ("with" ~> e) ^^ { case e1 ~ e2 => alg.TmTry(e1, e2) }
      ).reduce((a, b) => a ||| b)
    }
  }
}

trait FullErrorLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends FullRefLNG[E, T, L] with FullError.Lexer {
  val pFullErrorE = new FullError.Parser[E, L](){}
  val pFullErrorLNGE = pFullRefLNGE | pFullErrorE.pE
  val pFullErrorLNGT = pFullRefLNGT
}

object TestFullErrorLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends FullErrorLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pFullErrorLNGE(Test)(l), pFullErrorLNGT(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "\\x:Top.if error then (try x with true) else false",
      "error true",
      "(\\x:Bool.x) error"
    ).foreach(Test.parse)
  }
}