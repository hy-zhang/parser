package TAPL

import Util._

object Recon {

  trait Alg[E, T] extends Typed.Alg[E, T] with TyArith.Alg[E, T] with TypeVar.Alg[T]

  trait Print extends Alg[String, String] with Typed.Print with TyArith.Print with TypeVar.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends TyArith.Parser[E, T, L] with Typed.Parser[E, T, L] with TypeVar.Parser[T, L] {
    val pReconE = pTypedE | pTyArithE
    val pReconT = pTypedT | pTyArithT | pTypeVarT
  }

}

object TestRecon {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: Recon.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new Recon.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pReconE(alg)(l), lang.pReconT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new Recon.Print {})

  def main(args: Array[String]) = {
    List(
      "\\x:Bool. x",
      "(\\x:Bool->Bool. if x false then true else false)",
      "(\\x: Bool.if x then false else true)",
      "\\x: Nat.succ x",
      "(\\x: Nat.succ (succ x)) (succ 0)",
      "(\\x: X.\\y: X -> X.y x)",
      "(\\x: X -> X.x 0) (\\y: Nat.y)",
      "\\x:A.x"
    ).foreach(parseAndPrint)
  }
}