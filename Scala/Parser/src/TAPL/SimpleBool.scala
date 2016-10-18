package TAPL

import Util._

/* <5> */
object Typed {

  trait Alg[E, T] extends VarApp.Alg[E] {
    def TmAbs(x: String, t: T, e: E): E

    def TyArr(t1: T, t2: T): T
  }

  trait Print extends Alg[String, String] with VarApp.Print {
    def TmAbs(x: String, t: String, e: String) = "\\(" + x + ":" + t + ")." + e

    def TyArr(t1: String, t2: String) = t1 + "->" + t2
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends VarApp.Parser[E, F] {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

    private val pAbsE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      val e = l.pE
      val t = l.pT

      ("\\" ~> lcid) ~ (":" ~> t) ~ ("." ~> e) ^^ { case x ~ t0 ~ e0 => alg.TmAbs(x, t0, e0) } |||
        "(" ~> e <~ ")"
    }

    val pTypedE: Alg[E, T] => (=> F) => PackratParser[E] = pVarAppE | pAbsE

    val pTypedT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      val t = l.pT

      t ~ ("->" ~> t) ^^ { case t1 ~ t2 => alg.TyArr(t1, t2) } ||| "(" ~> t <~ ")"
    }
  }

}

object SimpleBool {

  trait Alg[E, T] extends Typed.Alg[E, T] with TypedBool.Alg[E, T]

  trait Print extends Alg[String, String] with Typed.Print with TypedBool.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Typed.Parser[E, T, L] with TypedBool.Parser[E, T, L] {
    val pSimpleBoolE = pTypedE | pTypedBoolE
    val pSimpleBoolT = pTypedT | pTypedBoolT
  }

}

object TestSimpleBool {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: SimpleBool.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new SimpleBool.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pSimpleBoolE(alg)(l), lang.pSimpleBoolT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new SimpleBool.Print {})

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