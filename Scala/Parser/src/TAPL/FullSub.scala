package TAPL

import Util._

trait FullSubParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends FullSimpleParser[E, T, L] with Top.Lexer {
  val pFullSubLNGE = pFullSimpleLNGE
  val pFullSubLNGT = pFullSimpleLNGT | new Top.Parser[T, L]() {}.pT
}

trait FullSubAlg[E, T] extends FullSimpleAlg[E, T] with Top.Alg[T]

trait FullSubPrint extends FullSubAlg[String, String] with FullSimplePrint with Top.Print

object TestFullSub {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullSubAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullSubParser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullSubLNGE(alg)(l), lang.pFullSubLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullSubPrint {})

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
