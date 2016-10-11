package TAPL

import Util._

/* <5> */
object SimpleBool {

  trait Alg[E, T] {
    def id(x: String): E

    def lam(x: String, t: T, e: E): E

    def app(e1: E, e2: E): E

    def arr(t1: T, t2: T): T
  }

  trait Print extends Alg[String, String] {
    def id(x: String) = x

    def lam(x: String, t: String, e: String) = "\\(" + x + ":" + t + ")." + e

    def app(e1: String, e2: String) = "[" + e1 + " " + e2 + "]"

    def arr(t1: String, t2: String) = t1 + "->" + t2
  }

  trait Lexer {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lazy val pE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        ident ^^ alg.id,
        ("\\" ~> lcid) ~ (":" ~> t) ~ ("." ~> e) ^^ { case x ~ t0 ~ e0 => alg.lam(x, t0, e0) },
        e ~ e ^^ { case e1 ~ e2 => alg.app(e1, e2) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }

    lazy val pT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      t ~ ("->" ~> t) ^^ { case t1 ~ t2 => alg.arr(t1, t2) }
    }
  }

}

trait SimpleBoolParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TyArithParser[E, T, L] with SimpleBool.Lexer {
  val pSimpleBoolET = new SimpleBool.Parser[E, T, L]() {}
  val pSimpleBoolLNGE = pTyArithLNGE | pSimpleBoolET.pE
  val pSimpleBoolLNGT = pTyArithLNGT | pSimpleBoolET.pT
}

trait SimpleBoolAlg[E, T] extends TyArithAlg[E, T] with SimpleBool.Alg[E, T]

trait SimpleBoolPrint extends SimpleBoolAlg[String, String] with TyArithPrint with SimpleBool.Print

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