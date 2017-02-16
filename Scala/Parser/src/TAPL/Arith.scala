package TAPL

import TAPL.Lib._


trait EParser[E] {
  val pE: Parser[E]
}


object Bool {

  trait Alg[E] {
    def TmTrue(): E

    def TmFalse(): E

    def TmIf(e1: E, e2: E, e3: E): E
  }

  trait Print extends Alg[String] {
    def TmTrue() = "true"

    def TmFalse() = "false"

    def TmIf(e1: String, e2: String, e3: String): String =
      "if (" + e1 + ") then (" + e2 + ") else (" + e3 + ")"
  }

  trait Parse[E] extends EParser[E] {
    lexical.reserved += ("true", "false", "if", "then", "else")
    lexical.delimiters += ("(", ")")

    val alg: Alg[E]

    val pBoolE: Parser[E] = {
      "true" ^^ { _ => alg.TmTrue() } |||
        "false" ^^ { _ => alg.TmFalse() } |||
        ("if" ~> pE) ~ ("then" ~> pE) ~ ("else" ~> pE) ^^ { case e1 ~ e2 ~ e3 => alg.TmIf(e1, e2, e3) } |||
        "(" ~> pE <~ ")"
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

    def TmSucc(e: String): String = "succ (" + e + ")"

    def TmPred(e: String): String = "pred (" + e + ")"

    def TmIsZero(e: String): String = "iszero (" + e + ")"
  }

  trait Parse[E] extends EParser[E] {
    lexical.reserved += ("iszero", "succ", "pred")
    lexical.delimiters += ("(", ")")

    val alg: Alg[E]

    val pNatE: Parser[E] = {
      def num(x: Int): E = x match {
        case 0 => alg.TmZero()
        case _ => alg.TmSucc(num(x - 1))
      }

      numericLit ^^ { x => num(x.toInt) } |||
        "succ" ~> pE ^^ alg.TmSucc |||
        "pred" ~> pE ^^ alg.TmPred |||
        "iszero" ~> pE ^^ alg.TmIsZero |||
        "(" ~> pE <~ ")"
    }
  }

}

object Arith {

  trait Alg[E] extends Bool.Alg[E] with Nat.Alg[E]

  trait Print extends Alg[String] with Bool.Print with Nat.Print

  trait Parse[E] extends Bool.Parse[E] with Nat.Parse[E] {
    override val alg: Alg[E]

    val pArithE: Parser[E] = pNatE ||| pBoolE

    override val pE: Parser[E] = pArithE
  }

}

object TestArith {

  def parseWithAlg[E](inp: String)(a: Arith.Alg[E]): E = {
    val p = new Arith.Parse[E] {
      override val alg: Arith.Alg[E] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new Arith.Print {}))
}