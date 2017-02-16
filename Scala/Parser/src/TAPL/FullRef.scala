package TAPL

import TAPL.Lib._


object Ref {

  trait Alg[E, T] {
    def TyRef(t: T): T

    def TmRef(e: E): E

    def TmDeRef(e: E): E

    def TmAssign(l: E, r: E): E
  }

  trait Print extends Alg[String, String] {
    def TyRef(t: String): String = "Ref " + t

    def TmRef(e: String): String = "ref " + e

    def TmDeRef(e: String): String = "!" + e

    def TmAssign(l: String, r: String): String = l + " := " + r
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("ref", "Ref")
    lexical.delimiters += ("!", ":=")

    val alg: Alg[E, T]

    val pRefE: Parser[E] = {
      "ref" ~> pE ^^ { e => alg.TmRef(e) } |||
        "!" ~> pE ^^ { e => alg.TmDeRef(e) } |||
        pE ~ (":=" ~> pE) ^^ { case lhs ~ rhs => alg.TmAssign(lhs, rhs) }
    }

    val pRefT: Parser[T] = "Ref" ~> pT ^^ alg.TyRef
  }

}

object SourceSink {

  trait Alg[T] {
    def TySource(t: T): T

    def TySink(t: T): T
  }

  trait Print extends Alg[String] {
    def TySource(t: String): String = "Source " + t

    def TySink(t: String): String = "Sink " + t
  }

  trait Parse[T] extends TParser[T] {
    lexical.reserved += ("Source", "Sink")

    val alg: Alg[T]

    val pSourceSinkT: Parser[T] =
      "Source" ~> pT ^^ alg.TySource |||
        "Sink" ~> pT ^^ alg.TySink
  }

}

object FullRef {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with TopBot.Alg[T] with Ref.Alg[E, T] with SourceSink.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with TopBot.Print with Ref.Print with SourceSink.Print

  trait Parse[E, T] extends FullSimple.Parse[E, T] with TopBot.Parse[T]
    with Ref.Parse[E, T] with SourceSink.Parse[T] {

    override val alg: Alg[E, T]

    val pFullRefE: Parser[E] = pFullSimpleE ||| pRefE
    val pFullRefT: Parser[T] = pFullSimpleT ||| pRefT ||| pTopBotT ||| pSourceSinkT

    override val pE: Parser[E] = pFullRefE
    override val pT: Parser[T] = pFullRefT
  }

}

object TestFullRef {

  def parseWithAlg[E, T](inp: String)(a: FullRef.Alg[E, T]): E = {
    val p = new FullRef.Parse[E, T] {
      override val alg: FullRef.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullRef.Print {}))

}