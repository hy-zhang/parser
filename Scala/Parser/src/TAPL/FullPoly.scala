package TAPL

import TAPL.Lib._


object Pack {

  trait Alg[E, T] {
    def TmPack(t1: T, e: E, t2: T): E

    def TmUnpack(tx: String, x: String, e1: E, e2: E): E
  }

  trait Print extends Alg[String, String] {
    override def TmPack(t1: String, e: String, t2: String): String = "{*" + t1 + "," + e + "} as " + t2

    override def TmUnpack(tx: String, x: String, e1: String, e2: String): String =
      "let {" + tx + "," + x + "} = " + e1 + " in " + e2
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("as", "let", "in")
    lexical.delimiters += (",", "{", "}", "*", "=")

    val alg: Alg[E, T]

    val pPackE: Parser[E] =
      ("{" ~> "*" ~> pT ~ ("," ~> pE) <~ "}") ~ ("as" ~> pT) ^^ { case t1 ~ ex ~ t2 => alg.TmPack(t1, ex, t2) } |||
        "let" ~> ("{" ~> ucid ~ ("," ~> lcid) <~ "}") ~ ("=" ~> pE) ~ ("in" ~> pE) ^^ { case tx ~ x ~ e1 ~ e2 => alg
          .TmUnpack(tx, x, e1, e2)
        }
  }

}

object Poly {

  trait Alg[E, T] {
    def TyAll(x: String, t: T): T

    def TySome(x: String, t: T): T

    def TmTAbs(x: String, e: E): E

    def TmTApp(e: E, t: T): E
  }

  trait Print extends Alg[String, String] {
    override def TyAll(x: String, t: String): String = "All " + x + "." + t

    override def TySome(x: String, t: String): String = "{Some " + x + "," + t + "}"

    override def TmTAbs(x: String, e: String): String = "\\" + x + "." + e

    override def TmTApp(e: String, t: String): String = e + " [" + t + "]"
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("All", "Some")
    lexical.delimiters += (".", ",", "{", "}", "[", "]")

    val alg: Alg[E, T]

    val pPolyE: Parser[E] =
      "\\" ~> ucid ~ ("." ~> pE) ^^ { case x ~ ex => alg.TmTAbs(x, ex) } |||
        pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => alg.TmTApp(ex, ty) }

    val pPolyT: Parser[T] =
      "All" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => alg.TyAll(x, ty) } |||
        ("{" ~> "Some" ~> ucid ~ ("," ~> pT) <~ "}") ^^ { case x ~ ty => alg.TySome(x, ty) }
  }

}

object FullPoly {

  trait Alg[E, T] extends Simple.Alg[E, T] with Poly.Alg[E, T] with Pack.Alg[E, T]

  trait Print extends Alg[String, String] with Simple.Print with Poly.Print with Pack.Print

  trait Parse[E, T] extends Simple.Parse[E, T] with Poly.Parse[E, T] with Pack.Parse[E, T] {
    override val alg: Alg[E, T]

    val pFullPolyE: Parser[E] = pSimpleE ||| pPolyE ||| pPackE
    val pFullPolyT: Parser[T] = pSimpleT ||| pPolyT

    override val pE: Parser[E] = pFullPolyE
    override val pT: Parser[T] = pFullPolyT
  }

}

object TestFullPoly {

  def parseWithAlg[E, T](inp: String)(a: FullPoly.Alg[E, T]): E = {
    val p = new FullPoly.Parse[E, T] {
      override val alg: FullPoly.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullPoly.Print {}))

}