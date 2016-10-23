package TAPL

import Util._

/* <7> */
object Ref {

  trait Alg[E, T] {
    def TyRef(t: T): T

    def TmRef(e: E): E

    def TmDeRef(e: E): E

    def TmAssign(l: E, r: E): E
  }

  trait Print extends Alg[String, String] {
    def TyRef(t: String) = "Ref " + t

    def TmRef(e: String) = "ref " + e

    def TmDeRef(e: String) = "!" + e

    def TmAssign(l: String, r: String) = l + " := " + r
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("ref", "Ref")
    lexical.delimiters += ("!", ":=")

    val pRefE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE

      "ref" ~> e ^^ { e => alg.TmRef(e) } |||
        "!" ~> e ^^ { e => alg.TmDeRef(e) } |||
        e ~ (":=" ~> e) ^^ { case lhs ~ rhs => alg.TmAssign(lhs, rhs) }
    }

    val pRefT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "Ref" ~> t ^^ alg.TyRef
    }
  }

}

object SourceSink {

  trait Alg[T] {
    def TySource(t: T): T

    def TySink(t: T): T
  }

  trait Print extends Alg[String] {
    def TySource(t: String) = "Source " + t

    def TySink(t: String) = "Sink " + t
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lexical.reserved += ("Source", "Sink")

    val pSourceSinkT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "Source" ~> t ^^ alg.TySource ||| "Sink" ~> t ^^ alg.TySink
    }
  }

}

object FullRef {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with TopBot.Alg[T] with Ref.Alg[E, T] with SourceSink.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with TopBot.Print with Ref.Print with SourceSink.Print

  trait Parser[E, T, L <: {val pE : PackratParser[E]; val pT : PackratParser[T]}]
    extends FullSimple.Parser[E, T, L] with TopBot.Parser[T, L] with Ref.Parser[E, T, L] with SourceSink.Parser[T, L] {
    val pFullRefE = pFullSimpleE | pRefE
    val pFullRefT = pFullSimpleT | pRefT | pTopBotT | pSourceSinkT
  }

}

object TestFullRef {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullRef.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullRef.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullRefE(alg)(l), lang.pFullRefT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullRef.Print {})

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