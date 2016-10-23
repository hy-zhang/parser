package TAPL

import Util._

object FullRecon {
  trait Alg[E, T] extends Recon.Alg[E, T] with Let.Alg[E]

  trait Print extends Alg[String, String] with Recon.Print with Let.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Recon.Parser[E, T, L] with Let.Parser[E, L] {
    val pFullReconE = pReconE | pLetE
    val pFullReconT = pReconT
  }

}

object TestFullRecon {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullRecon.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullRecon.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullReconE(alg)(l), lang.pFullReconT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullRecon.Print {})

  def main(args: Array[String]) = {
    List(
      "\\x:Bool. x",
      "(\\x:Bool->Bool. if x false then true else false)",
      "(\\x: Bool.if x then false else true)",
      "\\x: Nat.succ x",
      "(\\x: Nat.succ (succ x)) (succ 0)",
      "(\\x: X.\\y: X -> X.y x)",
      "(\\x: X -> X.x 0) (\\y: Nat.y)",
      "let f = \\x:A.x in true"
    ).foreach(parseAndPrint)
  }
}