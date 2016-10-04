package TAPL

/* <5> */
object SimpleBool {
  import Util._
  trait SimpleBoolAlg[E, T] {
    def trueV(): E
    def falseV(): E
    def ifS(e1: E, e2: E, e3: E): E
    def id(x: String): E
    def lam(x: String, t: T, e: E): E
    def app(e1: E, e2: E): E
    def bool(): T
    def arr(t1: T, t2: T): T
  }
  trait Lexer extends SimpleBoolAlg[String, String] {
    lexical.reserved += ("if", "then", "else", "true", "false", "Bool")
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")
    def trueV() = "true"
    def falseV() = "false"
    def ifS(e1: String, e2: String, e3: String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"
    def id(x: String) = x
    def lam(x: String, t: String, e: String) = "\\(" + x + ":" + t + ")." + e
    def app(e1: String, e2: String) = "[" + e1 + " " + e2 + "]"
    def bool() = "Bool"
    def arr(t1: String, t2: String) = t1 + "->" + t2
  }
  trait Parser[E, T, F <: {val pE: PackratParser[E]; val pT: PackratParser[T]}] {
    lazy val pE: SimpleBoolAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT
      "true" ^^ { _ => alg.trueV() } |||
        "false" ^^ { _ => alg.falseV() } |||
        ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => alg.ifS(e1, e2, e3) } |||
        ident ^^ alg.id |||
        ("\\" ~> lcid) ~ (":" ~> t) ~ ("." ~> e) ^^ { case x ~ t0 ~ e0 => alg.lam(x, t0, e0) } |||
        e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) } |||
        "(" ~> e <~ ")"
    }
    lazy val pT: SimpleBoolAlg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Bool" ^^ { _ => alg.bool() } |||
        t ~ ("->" ~> t) ^^ { case t1 ~ t2 => alg.arr(t1, t2) }
    }
  }
}

trait SimpleBoolLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends SimpleBool.Lexer {
  val pSimpleBoolET = new SimpleBool.Parser[E, T, L](){}
  val pSimpleBoolLNGE = pSimpleBoolET.pE
  val pSimpleBoolLNGT = pSimpleBoolET.pT
}

object TestSimpleBoolLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends SimpleBoolLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pSimpleBoolLNGE(Test)(l), pSimpleBoolLNGT(Test)(l))
    lazy val parse = runParser(fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "(\\x:Bool->Bool.x)",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)"
    ).foreach(Test.parse)
  }
}