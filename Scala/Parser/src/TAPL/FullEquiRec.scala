package TAPL

import TAPL.Util._

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
      lazy val t = l.pT

      "Rec" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyRec(x, ty) }
    }
  }

}

object FullEquiRec {

  trait Alg[E, T] extends FullSimple.Alg[E, T] with RecType.Alg[T]

  trait Print extends Alg[String, String] with FullSimple.Print with RecType.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends FullSimple.Parser[E, T, L] with RecType.Parser[T, L] {
    val pFullEquiRecE = pFullSimpleE
    val pFullEquiRecT = pFullSimpleT | pRecTypeT
  }

}

object TestFullEquiRec {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullEquiRec.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullEquiRec.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullEquiRecE(alg)(l), lang.pFullEquiRecT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullEquiRec.Print {})

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