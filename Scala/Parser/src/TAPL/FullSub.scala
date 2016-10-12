package TAPL

import Util._

object FullSub {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with Top.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with Top.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends FullSimple.Parser[E, T, L] with Top.Parser[T, L] {
    val pFullSubE = pFullSimpleE
    val pFullSubT = pFullSimpleT | pTopT
  }

}

object TestFullSub {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullSub.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullSub.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullSubE(alg)(l), lang.pFullSubT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullSub.Print {})

  def main(args: Array[String]) = {
    List(
      "x",
      "if x then false else x",
      "\\x:A.x",
      "\"hello\"",
      "0",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "true",
      "if false then true else false",
      "if (x) then true else false",
      "\\x:Bool.x",
      "(\\x:Bool->Bool.x)",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)",
      "\\x:Nat.succ x",
      "(\\x:Nat. succ (succ x)) (succ 0)",
      "\\f:T. \\x:Nat. f (f x)",
      "let x=true in x",
      "unit",
      "unit as Unit",
      "<l=unit> as <l:Unit, r:Unit>",
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}"
    ).foreach(parseAndPrint)
  }
}
