package TAPL

import Util._

/* <1> */
object Arith {

  trait Alg[E] {
    def trueV(): E

    def falseV(): E

    def ifS(e1: E, e2: E, e3: E): E

    def zero(): E

    def succ(e: E): E

    def pred(e: E): E

    def isZero(e: E): E
  }

  trait Print extends Alg[String] {
    def trueV() = "true"

    def falseV() = "false"

    def ifS(e1: String, e2: String, e3: String) = "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"

    def zero() = "0"

    def succ(e: String) = "succ (" + e + ")"

    def pred(e: String) = "pred (" + e + ")"

    def isZero(e: String) = "iszero (" + e + ")"
  }

  trait Lexer {
    lexical.reserved += ("true", "false", "if", "then", "else", "iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")
  }

  trait Parser[E, F <: {val pE : PackratParser[E]}] {
    lazy val pE: Alg[E] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      def num(x: Int): E = x match {
        case 0 => alg.zero()
        case _ => alg.succ(num(x - 1))
      }

      List(
        "true" ^^ { _ => alg.trueV() },
        "false" ^^ { _ => alg.falseV() },
        ("if" ~> e) ~ ("then" ~> e) ~ ("else" ~> e) ^^ { case e1 ~ e2 ~ e3 => alg.ifS(e1, e2, e3) },
        numericLit ^^ { x => num(x.toInt) },
        "succ" ~> e ^^ alg.succ,
        "pred" ~> e ^^ alg.pred,
        "iszero" ~> e ^^ alg.isZero,
        "(" ~> e <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait ArithParser[E, L <: {val pE : Util.PackratParser[E]}] extends Arith.Lexer {
  val pArithE = new Arith.Parser[E, L]() {}
  val pArithLNGE = pArithE.pE
  // we cannot use pE, such a name has incompatible types when overridden. is there a solution?
}

trait ArithAlg[E] extends Arith.Alg[E]

trait ArithPrint extends ArithAlg[String] with Arith.Print

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