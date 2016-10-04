package TAPL

/* <2> */
object Untyped {
  import Util._
  trait UntypedAlg[E] {
    def id(x : String) : E
    def lam(x : String, e : E) : E
    def app(e1 : E, e2 : E) : E
  }
  trait Lexer extends UntypedAlg[String] {
    lexical.delimiters += ("\\", ".", "(", ")")
    def id(x : String) = x
    def lam(x : String, e : String) = "\\" + x + "." + e
    def app(e1 : String, e2 : String) = "[" + e1 + " " + e2 + "]"
  }
  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE : UntypedAlg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      ident ^^ alg.id |||
      ("\\" ~> lcid) ~ ("." ~> e) ^^ { case x ~ e0 => alg.lam(x, e0) } |||
      e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
      "(" ~> e <~ ")"
    }
  }
}

trait UntypedLNG[E, L <: {val pE : Util.PackratParser[E]}] extends ArithLNG[E, L] with Untyped.Lexer {
  val pUntypedE : Untyped.Parser[E, L] = new Untyped.Parser[E, L](){}
  val pUntypedLNGE = pArithLNGE | pUntypedE.pE
}

object TestUntypedLNG {
  import Util._
  class List[E](pe : PackratParser[E]) { val pE = pe }
  object Test extends UntypedLNG[String, List[String]] {
    lazy val parser : (=> List[String]) => List[String] = l =>
      new List[String](pUntypedLNGE(Test)(l))
    lazy val parse = runParser(fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "\\x.x (x \\x.x)",
      "if pred 1 then false else succ x",
      "if \\x.x x x then if true then false else 0 else false"
    ).foreach(Test.parse)
  }
}