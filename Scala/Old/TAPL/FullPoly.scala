package TAPL

import TAPL.Util._

/* <17> */
object Pack {

  trait Alg[E, T] {
    def TmPack(t1: T, e: E, t2: T): E

    def TmUnpack(tx: String, x: String, e1: E, e2: E): E
  }

  trait Print extends Alg[String, String] {
    override def TmPack(t1: String, e: String, t2: String): String = "{*" + t1 + "," + e + "} as " + t2

    override def TmUnpack(tx: String, x: String, e1: String, e2: String): String = "let {" + tx + "," + x + "} = " +
      e1 + " in " + e2
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("as", "let", "in")
    lexical.delimiters += (",", "{", "}", "*", "=")

    val pPackE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      ("{" ~> "*" ~> t ~ ("," ~> e) <~ "}") ~ ("as" ~> t) ^^ { case t1 ~ ex ~ t2 => alg.TmPack(t1, ex, t2) } |||
        "let" ~> ("{" ~> ucid ~ ("," ~> lcid) <~ "}") ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case tx ~ x ~ e1 ~ e2 => alg
          .TmUnpack(tx, x, e1, e2)
        }
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

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("All", "Some")
    lexical.delimiters += (".", ",", "{", "}", "[", "]")

    val pPolyE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      "\\" ~> ucid ~ ("." ~> e) ^^ { case x ~ ex => alg.TmTAbs(x, ex) } |||
        e ~ ("[" ~> t <~ "]") ^^ { case ex ~ ty => alg.TmTApp(ex, ty) }
    }

    val pPolyT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "All" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyAll(x, ty) } |||
        ("{" ~> "Some" ~> ucid ~ ("," ~> t) <~ "}") ^^ { case x ~ ty => alg.TySome(x, ty) }
    }
  }

}

object FullPoly {

  trait Alg[E, T] extends Simple.Alg[E, T] with Poly.Alg[E, T] with Pack.Alg[E, T]

  trait Print extends Alg[String, String] with Simple.Print with Poly.Print with Pack.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Simple.Parser[E, T, L] with Poly.Parser[E, T, L] with Pack.Parser[E, T, L] {
    val pFullPolyE = pSimpleE | pPolyE | pPackE
    val pFullPolyT = pSimpleT | pPolyT
  }

}

object TestFullPoly {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullPoly.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullPoly.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullPolyE(alg)(l), lang.pFullPolyT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullPoly.Print {})

}