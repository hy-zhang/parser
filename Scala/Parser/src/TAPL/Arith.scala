package TAPL

import Util._

/* <1> */

object Bool {

  trait Alg[E] {
    def TmTrue(): E

    def TmFalse(): E

    def TmIf(e1: E, e2: E, e3: E): E
  }

  trait Print extends Alg[String] {
    def TmTrue() = "true"

    def TmFalse() = "false"

    def TmIf(e1: String, e2: String, e3: String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.reserved += ("true", "false", "if", "then", "else")
    lexical.delimiters += ("(", ")")

    lazy val pBoolE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      List(
        "true" ^^ { _ => alg.TmTrue() },
        "false" ^^ { _ => alg.TmFalse() },
        ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => alg.TmIf(e1, e2, e3) },
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

object Nat {

  trait Alg[E] {
    def TmZero(): E

    def TmSucc(e: E): E

    def TmPred(e: E): E

    def TmIsZero(e: E): E
  }

  trait Print extends Alg[String] {
    def TmZero() = "0"

    def TmSucc(e: String) = "succ (" + e + ")"

    def TmPred(e: String) = "pred (" + e + ")"

    def TmIsZero(e: String) = "iszero (" + e + ")"
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lexical.reserved += ("iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")

    lazy val pNatE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      def num(x: Int): E = x match {
        case 0 => alg.TmZero()
        case _ => alg.TmSucc(num(x - 1))
      }

      List(
        numericLit ^^ { x => num(x.toInt) },
        "succ" ~> e ^^ alg.TmSucc,
        "pred" ~> e ^^ alg.TmPred,
        "iszero" ~> e ^^ alg.TmIsZero,
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait ArithParser[E, L <: {val pE : Util.PackratParser[E]}] extends Bool.Parser[E, L] with Nat.Parser[E, L] {
  val pArithLNGE = pBoolE | pNatE
  // we cannot use pE, such a name has incompatible types when overridden. is there a solution?
}

trait ArithAlg[E] extends Bool.Alg[E] with Nat.Alg[E]

trait ArithPrint extends ArithAlg[String] with Bool.Print with Nat.Print

object TestArith {

  class List[E](pe: PackratParser[E]) {
    val pE = pe
  }

  def parse[E](inp: String)(alg: ArithAlg[E]) = {
    def parser(l: => List[E]): List[E] = {
      val lang = new ArithParser[E, List[E]] {}
      new List[E](lang.pArithLNGE(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new ArithPrint {})

  def main(args: Array[String]) = {
    List(
      "true",
      "if false then true else false",
      "3",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))"
    ).foreach(parseAndPrint)
  }
}