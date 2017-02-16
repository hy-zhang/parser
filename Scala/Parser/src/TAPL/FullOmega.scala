package TAPL

import TAPL.Lib._


trait KParser[K] {
  val pK: Parser[K]
}

trait ETKParser[E, T, K] extends ETParser[E, T] with KParser[K]

object Omega {

  trait Alg[E, T, K] {
    def KnStar(): K

    def KnArr(k1: K, k2: K): K

    def TyAll(x: String, k: K, t: T): T

    def TySome(x: String, k: K, t: T): T

    def TmTAbs(x: String, k: K, e: E): E

    def TmTApp(e: E, t: T): E

    def TyAbs(x: String, k: K, t: T): T

    def TyApp(t1: T, t2: T): T
  }

  trait Print extends Alg[String, String, String] {
    def KnStar(): String = "Star"

    def KnArr(k1: String, k2: String): String = k1 + "=>" + k2

    def TyAll(x: String, k: String, t: String): String = "All " + x + ":" + k + "." + t

    def TySome(x: String, k: String, t: String): String = "{Some " + x + ":" + k + "," + t + "}"

    def TmTAbs(x: String, k: String, e: String): String = "\\(" + x + ":" + k + ")." + e

    def TmTApp(e: String, t: String): String = e + " [" + t + "]"

    def TyAbs(x: String, k: String, t: String): String = "\\(" + x + ":" + k + ")." + t

    def TyApp(t1: String, t2: String): String = "[" + t1 + " " + t2 + "]"
  }

  trait Parse[E, T, K] extends ETKParser[E, T, K] {
    lexical.reserved += ("Star", "All", "Some")
    lexical.delimiters += ("=>", ":", ".", ",", "{", "}")

    val alg: Alg[E, T, K]

    val pOmegaE: Parser[E] =
      "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pE) ^^ { case x ~ kn ~ ex => alg.TmTAbs(x, kn, ex) } |||
        pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => alg.TmTApp(ex, ty) }

    val pOmegaT: Parser[T] =
      "All" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => alg.TyAll(x, kn, ty) } |||
        ("{" ~> "Some" ~> ucid ~ (":" ~> pK) ~ ("," ~> pT) <~ "}") ^^ { case x ~ kn ~ ty => alg.TySome(x, kn, ty) } |||
        "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => alg.TyAbs(x, kn, ty) } |||
        pT ~ pT ^^ { case t1 ~ t2 => alg.TyApp(t1, t2) }

    val pOmegaK: Parser[K] =
      "Star" ^^ { _ => alg.KnStar() } |||
        pK ~ ("=>" ~> pK) ^^ { case k1 ~ k2 => alg.KnArr(k1, k2) } |||
        "(" ~> pK <~ ")"
  }

}

object FullOmega {

  trait Alg[E, T, K] extends Simple.Alg[E, T] with Pack.Alg[E, T] with Ref.Alg[E, T] with Omega.Alg[E, T, K]

  trait Print extends Alg[String, String, String] with Simple.Print with Pack.Print with Ref.Print with Omega.Print

  trait Parse[E, T, K] extends Simple.Parse[E, T] with Pack.Parse[E, T]
    with Ref.Parse[E, T] with Omega.Parse[E, T, K] {

    override val alg: Alg[E, T, K]

    val pFullOmegaE: Parser[E] = pSimpleE ||| pPackE ||| pRefE ||| pOmegaE
    val pFullOmegaT: Parser[T] = pSimpleT ||| pRefT ||| pOmegaT
    val pFullOmegaK: Parser[K] = pOmegaK

    override val pE: Parser[E] = pFullOmegaE
    override val pT: Parser[T] = pFullOmegaT
    override val pK: Parser[K] = pFullOmegaK
  }

}

object TestFullOmega {

  def parseWithAlg[E, T, K](inp: String)(a: FullOmega.Alg[E, T, K]): E = {
    val p = new FullOmega.Parse[E, T, K] {
      override val alg: FullOmega.Alg[E, T, K] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullOmega.Print {}))

}