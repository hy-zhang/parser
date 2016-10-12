package TAPL

import Util._

trait ReconParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends TyArithParser[E, T, L] with Typed.Parser[E, T, L] with TypeVar.Parser[T, L] {
  val pReconLNGE = pTypedE | pTyArithLNGE
  val pReconLNGT = pTypedT | pTyArithLNGT | pTypeVarT
}

trait ReconAlg[E, T] extends Typed.Alg[E, T] with TyArithAlg[E, T] with TypeVar.Alg[T]

trait ReconPrint extends ReconAlg[String, String]
  with Typed.Print with TyArithPrint with TypeVar.Print

object TestRecon {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: ReconAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new ReconParser[E, T, List[E, T]] {}
      new List[E, T](lang.pReconLNGE(alg)(l), lang.pReconLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new ReconPrint {})

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