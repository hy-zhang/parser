package TAPL

import TAPL.Util._

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

    def TyAbs(x: String, k: String, t: String) = "\\(" + x + ":" + k + ")." + t

    def TyApp(t1: String, t2: String) = "[" + t1 + " " + t2 + "]"
  }

  trait Parser[E, T, K, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]; val pK : PackratParser[K]}] {
    lexical.reserved += ("Star", "All", "Some")
    lexical.delimiters += ("=>", ":", ".", ",", "{", "}")

    val pOmegaE: Alg[E, T, K] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT
      lazy val k = l.pK

      "\\" ~> ucid ~ (":" ~> k) ~ ("." ~> e) ^^ { case x ~ kn ~ ex => alg.TmTAbs(x, kn, ex) } |||
        e ~ ("[" ~> t <~ "]") ^^ { case ex ~ ty => alg.TmTApp(ex, ty) }
    }

    val pOmegaT: Alg[E, T, K] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      lazy val k = l.pK

      List(
        "All" ~> ucid ~ (":" ~> k) ~ ("." ~> t) ^^ { case x ~ kn ~ ty => alg.TyAll(x, kn, ty) },
        ("{" ~> "Some" ~> ucid ~ (":" ~> k) ~ ("," ~> t) <~ "}") ^^ { case x ~ kn ~ ty => alg.TySome(x, kn, ty) },
        "\\" ~> ucid ~ (":" ~> k) ~ ("." ~> t) ^^ { case x ~ kn ~ ty => alg.TyAbs(x, kn, ty) },
        t ~ t ^^ { case t1 ~ t2 => alg.TyApp(t1, t2) }
      ).reduce((a, b) => a ||| b)
    }

    val pOmegaK: Alg[E, T, K] => (=> F) => PackratParser[K] = alg => l => {
      lazy val k = l.pK

      "Star" ^^ { _ => alg.KnStar() } |||
        k ~ ("=>" ~> k) ^^ { case k1 ~ k2 => alg.KnArr(k1, k2) } |||
        "(" ~> k <~ ")"
    }
  }

}

object FullOmega {

  trait Alg[E, T, K] extends Simple.Alg[E, T] with Pack.Alg[E, T] with Ref.Alg[E, T] with Omega.Alg[E, T, K]

  trait Print extends Alg[String, String, String] with Simple.Print with Pack.Print with Ref.Print with Omega.Print

  trait Parser[E, T, K, L <: {val pE : PackratParser[E]; val pT : PackratParser[T]; val pK : PackratParser[K]}]
    extends Simple.Parser[E, T, L] with Pack.Parser[E, T, L] with Ref.Parser[E, T, L] with Omega.Parser[E, T, K, L] {
    val pFullOmegaE = pSimpleE | pPackE | pRefE | pOmegaE
    val pFullOmegaT = pSimpleT | pRefT | pOmegaT
    val pFullOmegaK = pOmegaK
  }

}

object TestFullOmega {

  class List[E, T, K](pe: PackratParser[E], pt: PackratParser[T], pk: PackratParser[K]) {
    val pE = pe
    val pT = pt
    val pK = pk
  }

  def parse[E, T, K](inp: String)(alg: FullOmega.Alg[E, T, K]) = {
    def parser(l: => List[E, T, K]): List[E, T, K] = {
      val lang = new FullOmega.Parser[E, T, K, List[E, T, K]] {}
      new List[E, T, K](lang.pFullOmegaE(alg)(l), lang.pFullOmegaT(alg)(l), lang.pFullOmegaK(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullOmega.Print {})

  def main(args: Array[String]) = {
    List(
      "x",
      "if x then false else x",
      "\\x:A.x",
      "\\X:Star=>Star.unit as X Bool",
      "(\\X:Star.\\x:X.x) [All X:Star.X->X]",
      "\\x:({Some X:Star, {c:X, f:X->Nat}}).x",
      "{*All Y:Star.Y, \\x:All Y:Star.Y.x} as {Some X:Star, X->X}",
      "{*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
    ).foreach(parseAndPrint)
  }
}