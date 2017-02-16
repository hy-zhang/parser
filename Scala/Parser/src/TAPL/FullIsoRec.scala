package TAPL

import TAPL.Lib._


object Fold {

  trait Alg[E, T] {
    def TmFold(e: E, t: T): E

    def TmUnfold(e: E, t: T): E
  }

  trait Print extends Alg[String, String] {
    def TmFold(e: String, t: String): String = "fold [" + t + "] " + e

    def TmUnfold(e: String, t: String): String = "unfold [" + t + "] " + e
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("fold", "unfold")
    lexical.delimiters += ("[", "]")

    val alg: Alg[E, T]

    val pFoldE: Parser[E] =
      "fold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => alg.TmFold(ex, ty) } |||
        "unfold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => alg.TmUnfold(ex, ty) }
  }

}

object FullIsoRec {

  trait Alg[E, T] extends FullEquiRec.Alg[E, T] with Fold.Alg[E, T]

  trait Print extends Alg[String, String] with FullEquiRec.Print with Fold.Print

  trait Parse[E, T] extends FullEquiRec.Parse[E, T] with Fold.Parse[E, T] {
    override val alg: Alg[E, T]

    val pFullIsoRecE: Parser[E] = pFullEquiRecE ||| pFoldE
    val pFullIsoRecT: Parser[T] = pFullEquiRecT

    override val pE: Parser[E] = pFullIsoRecE
    override val pT: Parser[T] = pFullIsoRecT
  }

}

object TestFullIsoRec {

  def parseWithAlg[E, T](inp: String)(a: FullIsoRec.Alg[E, T]): E = {
    val p = new FullIsoRec.Parse[E, T] {
      override val alg: FullIsoRec.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullIsoRec.Print {}))

}