package TAPL


object FullPoly {

  import Util._

  trait Lexer {
    lexical.reserved += ("All", "Some", "as", "let", "in")
    lexical.delimiters += (".", ",", "{", "}", "*")
  }

  type ParserSig[E, T] = {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
  }

  trait FullPolyAlg[E, T] {
    def TyAll(x: String, t: T): T

    def TySome(x: String, t: T): T

    def TmTAbs(x: String, e: E): E

    def TmTApp(e: E, t: T): E

    def TmPack(t1: T, e: E, t2: T): E

    def TmUnpack(tx: String, x: String, e1: E, e2: E): E
  }

  trait PrettyPrint extends FullPolyAlg[String, String] {
    override def TyAll(x: String, t: String): String = "All " + x + "." + t

    override def TySome(x: String, t: String): String = "{Some " + x + "," + t + "}"

    override def TmTAbs(x: String, e: String): String = "\\" + x + "." + e

    override def TmTApp(e: String, t: String): String = e + " [" + t + "]"

    override def TmPack(t1: String, e: String, t2: String): String = "{*" + t1 + "," + e + "} as " + t2

    override def TmUnpack(tx: String, x: String, e1: String, e2: String): String = "let {" + tx + "," + x + "} = " + e1 + " in " + e2
  }

  trait Parser[E, T, F <: ParserSig[E, T]] {
    lazy val pE: FullPolyAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        "\\" ~> ucid ~ ("." ~> e) ^^ { case x ~ ex => alg.TmTAbs(x, ex) },
        e ~ ("[" ~> t <~ "]") ^^ { case ex ~ ty => alg.TmTApp(ex, ty) },
        ("{" ~> "*" ~> t ~ ("," ~> e) <~ "}") ~ ("as" ~> t) ^^ { case t1 ~ ex ~ t2 => alg.TmPack(t1, ex, t2) },
        "let" ~> ("{" ~> ucid ~ ("," ~> lcid) <~ "}") ~ ("=" ~> e) ~ ("in" ~> e) ^^ { case tx ~ x ~ e1 ~ e2 => alg.TmUnpack(tx, x, e1, e2) }
      ).reduce((a, b) => a ||| b)
    }

    lazy val pT: FullPolyAlg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      List(
        "All" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyAll(x, ty) },
        ("{" ~> "Some" ~> ucid ~ ("," ~> t) <~ "}")^^ { case x ~ ty => alg.TySome(x, ty) }
      ).reduce((a, b) => a ||| b)
    }
  }

  def pE[F <: ParserSig[String, String]] = {
    new Parser[String, String, F]() {}.pE(new PrettyPrint() {})
  }

  def pT[F <: ParserSig[String, String]] = {
    new Parser[String, String, F]() {}.pT(new PrettyPrint() {})
  }
}

object TestFullPoly extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer with FullError.Lexer with RcdSubBot.Lexer with FullIsoRec.Lexer with FullPoly.Lexer {

  import Util._

  object parser {

    trait ParserSig[E, T] {
      val pE: PackratParser[E]
      val pT: PackratParser[T]
    }

    type R = ParserSig[String, String]

    val p: (=> R) => R = l => new R() {
      override lazy val pE = List(
        Arith.pE[R], FullUntyped.pE[R], SimpleBool.pE[R], FullSimple.pE[R], FullRef.pE[R], FullError.pE[R], FullIsoRec.pE[R], FullPoly.pE[R]
      ).reduce(alt[String, R])(l)

      override lazy val pT = List(
        TyArith.pT[R], SimpleBool.pT[R], FullSimple.pT[R], FullRef.pT[R], RcdSubBot.pT[R], FullPoly.pT[R]
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
      "(\\X.\\x:X.x) [All X.X->X]",
      "\\x:({Some X, {c:X, f:X->Nat}}).x",
      "{*All Y.Y, \\x:(All Y.Y).x} as {Some X, X->X}",
      "{*Nat, {c=0, f=\\x:Nat. succ x}} as {Some X, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ x}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)"
    ).foreach(parse)
  }
}