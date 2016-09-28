package TAPL

/* <5> */
object SimpleBool {

  import Util._

  trait Lexer {
    lexical.reserved += ("if", "then", "else", "true", "false", "Bool")
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")
  }

  type ParserT[E, T] = {val pE: PackratParser[E]; val pT: PackratParser[T]}

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

  trait Pretty extends SimpleBoolAlg[String, String] {
    def trueV() = "true"

    def falseV() = "false"

    def ifS(e1: String, e2: String, e3: String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"

    def id(x: String) = x

    def lam(x: String, t: String, e: String) = "\\(" + x + ":" + t + ")." + e

    def app(e1: String, e2: String) = "[" + e1 + " " + e2 + "]"

    def bool() = "Bool"

    def arr(t1: String, t2: String) = t1 + "->" + t2
  }

  trait Parser[E, T, F <: ParserT[E, T]] {
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

  def pET[F <: ParserT[String, String]] = {
    (new Parser[String, String, F]() {}.pE(new Pretty() {}),
      new Parser[String, String, F]() {}.pT(new Pretty() {}))
  }

  def pE[F <: ParserT[String, String]] = {
    new Parser[String, String, F]() {}.pE(new Pretty() {})
  }

  def pT[F <: ParserT[String, String]] = {
    new Parser[String, String, F]() {}.pT(new Pretty() {})
  }
}

object TestSimpleBool extends SimpleBool.Lexer {

  import Util._

  trait List[E, T] {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
  }

  type L = List[String, String]

  trait Parse[E, T] {
    type L = List[E, T]
    type PE = (=> L) => PackratParser[E]
    type PT = (=> L) => PackratParser[T]
    val pSimpleBoolET: (PE, PT)
    val p: (=> L) => L = l => new L() {
      override lazy val pE = pSimpleBoolET._1(l)
      override lazy val pT = pSimpleBoolET._2(l)
    }
  }

  lazy val parser = new Parse[String, String]() {
    lazy val pSimpleBoolET = SimpleBool.pET[L]
  }
  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    parse("(\\x:Bool->Bool.x)")
    parse("(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)")
  }
}
