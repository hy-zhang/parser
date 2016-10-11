package TAPL

import Util._

/* <5> */
//todo
object TypedAbsArr {

  trait Alg[E, T] {
    def lam(x: String, t: T, e: E): E

    def arr(t1: T, t2: T): T
  }

  trait Print extends Alg[String, String] {
    def lam(x: String, t: String, e: String) = "\\(" + x + ":" + t + ")." + e

    def arr(t1: String, t2: String) = t1 + "->" + t2
  }

  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lazy val pE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      ("\\" ~> lcid) ~ (":" ~> t) ~ ("." ~> e) ^^ { case x ~ t0 ~ e0 => alg.lam(x, t0, e0) } |||
        "(" ~> e <~ ")"
    }

    lazy val pT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      t ~ ("->" ~> t) ^^ { case t1 ~ t2 => alg.arr(t1, t2) } ||| "(" ~> t <~ ")"
    }
  }

}

trait TypedParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends VarApp.Lexer with TypedAbsArr.Lexer {
  val pTypedET = new TypedAbsArr.Parser[E, T, L]() {}
  val pTypedLNGE = new VarApp.Parser[E, L]() {}.pE | pTypedET.pE
  val pTypedLNGT = pTypedET.pT
}

trait TypedAlg[E, T] extends VarApp.Alg[E] with TypedAbsArr.Alg[E, T]

trait TypedPrint extends TypedAlg[String, String] with VarApp.Print with TypedAbsArr.Print

trait SimpleBoolParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TypedParser[E, T, L] with Bool.Lexer with BoolType.Lexer {
  val pSimpleBoolLNGE = pTypedLNGE | new Bool.Parser[E, L]() {}.pE
  val pSimpleBoolLNGT = pTypedLNGT | new BoolType.Parser[T, L]() {}.pT
}

trait SimpleBoolAlg[E, T] extends TypedAlg[E, T] with Bool.Alg[E] with BoolType.Alg[T]

trait SimpleBoolPrint extends SimpleBoolAlg[String, String]
  with TypedPrint with Bool.Print with BoolType.Print

object TestSimpleBool {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: SimpleBoolAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new SimpleBoolParser[E, T, List[E, T]] {}
      new List[E, T](lang.pSimpleBoolLNGE(alg)(l), lang.pSimpleBoolLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new SimpleBoolPrint {})

  def main(args: Array[String]) = {
    List(
      "true",
      "if false then true else false",
      "if x then true else false",
      "\\x:Bool.x",
      "(\\x:Bool->Bool.x)",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)"
    ).foreach(parseAndPrint)
  }
}