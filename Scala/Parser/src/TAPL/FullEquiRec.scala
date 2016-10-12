package TAPL

import Util._

/* <12> */
object RecType {

  trait Alg[T] {
    def TyRec(x: String, t: T): T
  }

  trait Print extends Alg[String] {
    def TyRec(x: String, t: String) = "Rec " + x + "." + t
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    lexical.reserved += "Rec"
    lexical.delimiters += "."

    val pRecTypeT: Alg[T] => (=> F) => PackratParser[T] = alg => l => {
      val t = l.pT

      "Rec" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyRec(x, ty) }
    }
  }

}

trait FullEquiRecParser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
  extends FullSimpleParser[E, T, L] with RecType.Parser[T, L] {
  val pFullEquiRecLNGE = pFullSimpleLNGE
  val pFullEquiRecLNGT = pFullSimpleLNGT | pRecTypeT
}

trait FullEquiRecAlg[E, T] extends FullSimpleAlg[E, T] with RecType.Alg[T]

trait FullEquiRecPrint extends FullEquiRecAlg[String, String] with FullSimplePrint with RecType.Print

object TestFullEquiRec {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullEquiRecAlg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullEquiRecParser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullEquiRecLNGE(alg)(l), lang.pFullEquiRecLNGT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullEquiRecPrint {})

  def main(args: Array[String]) = {
    List(
      "\\f:(Rec X.A->A).\\x:A.f x",
      "\\x:<a:Bool, b:Bool>.x",
      "\\x:(Rec P.{get:Nat, inc:Unit->P}).x",
      "\\x:(Rec A.Nat->A).x",
      "let g = fix (\\f:Nat->(Rec A.Nat->A).\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.if iszero m then n else succ (p (pred m) n))"
    ).foreach(parseAndPrint)
  }
}