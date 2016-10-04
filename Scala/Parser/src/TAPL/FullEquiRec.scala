package TAPL

/* <12> */
object FullEquiRec {
  import Util._
  trait FullEquiRecAlg[T] {
    def TyRec(x: String, t: T): T
  }
  trait Lexer extends FullEquiRecAlg[String] {
    lexical.reserved += "Rec"
    lexical.delimiters += "."
    def TyRec(x: String, t: String) = "Rec " + x + "." + t
  }
  trait Parser[T, F <: {val pT: PackratParser[T]}] {
    lazy val pT: FullEquiRecAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Rec" ~> ucid ~ ("." ~> t) ^^ { case x ~ ty => alg.TyRec(x, ty) }
    }
  }
}

trait FullEquiRecLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends FullErrorLNG[E, T, L] with RcdSubBot.Lexer with FullEquiRec.Lexer {
  val pRcdSubBotT = new RcdSubBot.Parser[T, L](){}
  val pFullEquiRecT = new FullEquiRec.Parser[T, L](){}
  val pFullEquiRecLNGE = pFullErrorLNGE
  val pFullEquiRecLNGT = pFullErrorLNGT | pRcdSubBotT.pT | pFullEquiRecT.pT
}

object TestFullEquiRecLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends FullEquiRecLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pFullEquiRecLNGE(Test)(l), pFullEquiRecLNGT(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "\\f:(Rec X.A->A).\\x:A.f x",
      "\\x:<a:Bool, b:Bool>.x",
      "\\x:(Rec P.{get:Nat, inc:Unit->P}).x",
      "\\x:(Rec A.Nat->A).x",
      "let g = fix (\\f:Nat->(Rec A.Nat->A).\\n:Nat.f) in unit",
      "\\l:NList.case l of <nil=u> => true | <cons=p> => false",
      "fix (\\p:Nat->Nat->Nat.\\m:Nat.\\n:Nat.if iszero m then n else succ (p (pred m) n))"
    ).foreach(Test.parse)
  }
}