package TAPL

import Util._

/* <7> */
object FullRef {

  trait Alg[E, T] {
    def TyTop(): T

    def TyBot(): T

    def TyRef(t: T): T

    def TmRef(e: E): E

    def TmDeRef(e: E): E

    def TmAssign(l: E, r: E): E

    def TySource(t: T): T

    def TySink(t: T): T
  }

  trait Lexer {
    lexical.reserved += ("Top", "Bot", "ref", "Ref", "Source", "Sink")
    lexical.delimiters += ("!", ":=")
  }

  trait Print extends Alg[String, String] {
    def TyTop() = "Top"

    def TyBot() = "Bot"

    def TyRef(t: String) = "Ref " + t

    def TmRef(e: String) = "ref " + e

    def TmDeRef(e: String) = "!" + e

    def TmAssign(l: String, r: String) = l + " := " + r

    def TySource(t: String) = "Source " + t

    def TySink(t: String) = "Sink " + t
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lazy val pE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        "ref" ~> e ^^ { e => alg.TmRef(e) },
        "!" ~> e ^^ { e => alg.TmDeRef(e) },
        e ~ (":=" ~> e) ^^ { case lhs ~ rhs => alg.TmAssign(lhs, rhs) }
      ).reduce((a, b) => a ||| b)
    }

    lazy val pT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      List(
        "Top" ^^ { _ => alg.TyTop() },
        "Bot" ^^ { _ => alg.TyBot() },
        "Ref" ~> t ^^ alg.TyRef,
        "Source" ~> t ^^ alg.TySource,
        "Sink" ~> t ^^ alg.TySink,
        "(" ~> t <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

}

trait FullRefParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends FullSimpleParser[E, T, L] with FullRef.Lexer {
  val pFullRefET = new FullRef.Parser[E, T, L]() {}
  val pFullRefLNGE = pFullSimpleLNGE | pFullRefET.pE
  val pFullRefLNGT = pFullSimpleLNGT | pFullRefET.pT
}

trait FullRefAlg[E, T] extends FullSimpleAlg[E, T] with FullRef.Alg[E, T]

trait FullRefPrint extends FullRefAlg[String, String] with FullSimplePrint with FullRef.Print

object TestFullRef {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullRefAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullRefParser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullRefLNGE(alg)(l), lang.pFullRefLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullRefPrint {})

  def main(args: Array[String]) = {
    List(
      "\\a:Ref (Nat->Nat).\\n:Nat.(!a n)",
      "\\a:Unit.ref (\\n:Nat.0)",
      "\\a:Ref (Nat->Nat).\\m:Nat.\\n:Nat.let oldf = !a in a := (\\n:Nat.if true then v else (oldf n))",
      "\\x:Sink Bool.unit",
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)"
    ).foreach(parseAndPrint)
  }
}