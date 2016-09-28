package TAPL


object FullEquiRec {

  import Util._

  trait Lexer {
    lexical.reserved += "Rec"
    lexical.delimiters += "."
  }

  type ParserSig[T] = {
    val pT: PackratParser[T]
  }

  trait FullEquiRecAlg[T] {
    def TyRec(x: String, t: T): T
  }

  trait PrettyPrint extends FullEquiRecAlg[String] {
    def TyRec(x: String, t: String) = "Rec " + x + "." + t
  }

  trait Parser[T, F <: ParserSig[T]] {
    lazy val pT: FullEquiRecAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "Rec" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyRec(x, ty) }
    }
  }

  def pT[F <: ParserSig[String]] = {
    new Parser[String, F]() {}.pT(new PrettyPrint() {})
  }
}

object TestFullEquiRec extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer with FullError.Lexer with RcdSubBot.Lexer with FullEquiRec.Lexer {

  import Util._

  object parser {

    trait ParserSig[E, T] {
      val pE: PackratParser[E]
      val pT: PackratParser[T]
    }

    type R = ParserSig[String, String]

    lazy val pSimpleBoolET = SimpleBool.pET[R]
    lazy val pFullSimpleET = FullSimple.pET[R]

    val p: (=> R) => R = l => new R() {
      override lazy val pE = List(
        Arith.pE[R], FullUntyped.pE[R], pSimpleBoolET._1, pFullSimpleET._1, FullRef.pE[R], FullError.pE[R]
      ).reduce(alt[String, R])(l)

      override lazy val pT = List(
        TyArith.pT[R], pSimpleBoolET._2, pFullSimpleET._2, FullRef.pT[R], RcdSubBot.pT[R], FullEquiRec.pT[R]
      ).reduce(alt[String, R])(l)
    }
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    List(
      "\\f:(Rec X.A->A).\\x:A.f x",
      "\\x:<a:Bool, b:Bool>.x",
      "\\x:(Rec P.{get:Nat, inc:Unit->P}).x",
      "\\x:(Rec A.Nat->A).x",
      "let g = fix (\\f:Nat->(Rec A.Nat->A).\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.if iszero m then n else succ (p (pred m) n))"
    ).foreach(parse)
  }
}