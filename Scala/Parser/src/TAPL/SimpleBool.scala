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

  def pET[E, T, F <: ParserT[E, T]] = new Parser[E, T, F](){}
}

object TestSimpleBool extends SimpleBool.Lexer {
  import Util._
  trait List[E, T] { val pE: PackratParser[E]; val pT: PackratParser[T] }
  type L = List[String, String]

  trait Parse[E, T] {
    type L = List[E, T]
    val pSimpleBoolET = SimpleBool.pET[E, T, L]
    val pE = pSimpleBoolET.pE
    val pT = pSimpleBoolET.pT
  }
  trait Pretty extends SimpleBool.Pretty {}

  lazy val parser : (=> L) => L = l => new L() {
    lazy val pE = new Parse[String, String](){}.pE(new Pretty(){})(l)
    lazy val pT = new Parse[String, String](){}.pT(new Pretty(){})(l)
  }
  lazy val parse = runParser(fix(parser).pE)

  def main(args: Array[String]) = {
    parse("(\\x:Bool->Bool.x)")
    parse("(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)")
  }
}
