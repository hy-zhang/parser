package TAPL

import TAPL.Lib._


object RecType {

  trait Alg[T] {
    def TyRec(x: String, t: T): T
  }

  trait Print extends Alg[String] {
    def TyRec(x: String, t: String): String = "Rec " + x + "." + t
  }

  trait Parse[T] extends TParser[T] {
    lexical.reserved += "Rec"
    lexical.delimiters += "."

    val alg: Alg[T]

    val pRecTypeT: Parser[T] = "Rec" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => alg.TyRec(x, ty) }
  }

}

object FullEquiRec {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with RecType.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with RecType.Print

  trait Parse[E, T] extends FullSimple.Parse[E, T] with RecType.Parse[T] {
    override val alg: Alg[E, T]

    val pFullEquiRecE: Parser[E] = pFullSimpleE
    val pFullEquiRecT: Parser[T] = pFullSimpleT ||| pRecTypeT

    override val pE: Parser[E] = pFullEquiRecE
    override val pT: Parser[T] = pFullEquiRecT
  }

}

object TestFullEquiRec {

  def parseWithAlg[E, T](inp: String)(a: FullEquiRec.Alg[E, T]): E = {
    val p = new FullEquiRec.Parse[E, T] {
      override val alg: FullEquiRec.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullEquiRec.Print {}))

}