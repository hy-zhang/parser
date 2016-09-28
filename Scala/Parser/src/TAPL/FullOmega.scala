package TAPL


object FullOmega {

  import Util._

  trait Lexer {
    lexical.reserved += ("Star", "All", "Some")
    lexical.delimiters += ("=>", ":", ".", ",", "{", "}")
  }

  type ParserSig[E, T, K] = {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
    val pK: PackratParser[K]
  }

  trait FullOmegaAlg[E, T, K] {
    def KnStar(): K

    def KnArr(k1: K, k2: K): K

    def TyAll2(x: String, k: K, t: T): T

    def TySome2(x: String, k: K, t: T): T

    def TmTAbs2(x: String, k: K, e: E): E

    // Missed some cases?
  }

  trait PrettyPrint extends FullOmegaAlg[String, String, String] {
    override def KnStar(): String = "Star"

    override def KnArr(k1: String, k2: String): String = k1 + "=>" + k2

    override def TyAll2(x: String, k: String, t: String): String = "All " + x + ":" + k + "." + t

    override def TySome2(x: String, k: String, t: String): String = "{Some " + x + ":" + k + "," + t + "}"

    override def TmTAbs2(x: String, k: String, e: String): String = "\\(" + x + ":" + k + ")." + e
  }

  trait Parser[E, T, K, F <: ParserSig[E, T, K]] {
    lazy val pE: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val k = l.pK

      "\\" ~> ucid ~ (":" ~> k) ~ ("." ~> e) ^^ { case x ~ kn ~ ex => alg.TmTAbs2(x, kn, ex) }
    }

    lazy val pT: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      lazy val k = l.pK

      List(
        "All" ~> ucid ~ (":" ~> k) ~ ("." ~> t) ^^ { case x ~ kn ~ ty => alg.TyAll2(x, kn, ty) },
        ("{" ~> "Some" ~> ucid ~ (":" ~> k) ~ ("," ~> t) <~ "}") ^^ { case x ~ kn ~ ty => alg.TySome2(x, kn, ty) }
      ).reduce((a, b) => a ||| b)
    }

    lazy val pK: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[K] = alg => l => {
      lazy val k = l.pK

      List(
        "Star" ^^ { _ => alg.KnStar() },
        k ~ ("=>" ~> k) ^^ { case k1 ~ k2 => alg.KnArr(k1, k2) }
      ).reduce((a, b) => a ||| b)
    }
  }

  def pE[F <: ParserSig[String, String, String]] = {
    new Parser[String, String, String, F]() {}.pE(new PrettyPrint() {})
  }

  def pT[F <: ParserSig[String, String, String]] = {
    new Parser[String, String, String, F]() {}.pT(new PrettyPrint() {})
  }

  def pK[F <: ParserSig[String, String, String]] = {
    new Parser[String, String, String, F]() {}.pK(new PrettyPrint() {})
  }
}

object TestFullOmega extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer with FullError.Lexer with RcdSubBot.Lexer with FullIsoRec.Lexer with FullPoly.Lexer with FullOmega.Lexer {

  import Util._

  object parser {

    trait ParserSig[E, T, K] {
      val pE: PackratParser[E]
      val pT: PackratParser[T]
      val pK: PackratParser[K]
    }

    type R = ParserSig[String, String, String]

    val p: (=> R) => R = l => new R() {
      override lazy val pE = List(
        Arith.pE[R], FullUntyped.pE[R], SimpleBool.pE[R], FullSimple.pE[R], FullRef.pE[R], FullError.pE[R], FullIsoRec.pE[R], FullPoly.pE[R], FullOmega.pE[R]
      ).reduce(alt[String, R])(l)

      override lazy val pT = List(
        TyArith.pT[R], SimpleBool.pT[R], FullSimple.pT[R], FullRef.pT[R], RcdSubBot.pT[R], FullOmega.pT[R]
      ).reduce(alt[String, R])(l)

      override lazy val pK = List(
        FullOmega.pK[R]
      ).reduce(alt[String, R])(l)
    }
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    List(
      "fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get",
      "x",
      "if x then false else x",
      "\\x:A.x",
      "\\X:Star=>Star.X",
      "(\\X:Star.\\x:X.x) [All X:Star.X->X]",
      "\\x:({Some X:Star, {c:X, f:X->Nat}}).x",
      "{*All Y:Star.Y, \\x:All Y:Star.Y.x} as {Some X:Star, X->X}",
      "{*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
    ).foreach(parse)
  }
}