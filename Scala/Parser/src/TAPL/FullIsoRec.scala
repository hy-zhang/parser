package TAPL

import Util._

/* <13> */
object Fold {

  trait Alg[E, T] {
    def TmFold(e: E, t: T): E

    def TmUnfold(e: E, t: T): E
  }

  trait Print extends Alg[String, String] {
    def TmFold(e: String, t: String) = "fold [" + t + "] " + e

    def TmUnfold(e: String, t: String) = "unfold [" + t + "] " + e
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("fold", "unfold")
    lexical.delimiters += ("[", "]")

    val pFoldE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      val e = l.pE
      val t = l.pT

      List(
        "fold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmFold(ex, ty) },
        "unfold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmUnfold(ex, ty) }
      ).reduce((a, b) => a ||| b)
    }
  }

}

object FullIsoRec {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with Fold.Alg[E, T] with RecType.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with Fold.Print with RecType.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends FullSimple.Parser[E, T, L] with Fold.Parser[E, T, L] with RecType.Parser[T, L] {
    val pFullIsoRecE = pFullSimpleE | pFoldE
    val pFullIsoRecT = pFullSimpleT | pRecTypeT
  }

}

object TestFullIsoRec {

  import Util._

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullIsoRec.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullIsoRec.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullIsoRecE(alg)(l), lang.pFullIsoRecT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullIsoRec.Print {})

  def main(args: Array[String]) = {
    List(
      "\\f:(Rec X.A->A).\\x:A.f x",
      "\\x:<a:Bool, b:Bool>.x",
      "\\x:(Rec P.{get:Nat, inc:Unit->P}).x",
      "\\x:(Rec A.Nat->A).x",
      "let g = fix (\\f:Nat->(Rec A.Nat->A).\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.if iszero m then n else succ (p (pred m) n))",
      "fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get"
    ).foreach(parseAndPrint)
  }
}