package TAPL

/* <13> */
object FullIsoRec {
  import Util._
  trait FullIsoAlg[E, T] {
    def TmFold(e: E, t: T): E
    def TmUnfold(e: E, t: T): E
  }
  trait Lexer extends FullIsoAlg[String, String] {
    lexical.reserved += ("fold", "unfold")
    lexical.delimiters += ("[", "]")
    def TmFold(e: String, t: String) = "fold [" + t + "] " + e
    def TmUnfold(e: String, t: String) = "unfold [" + t + "] " + e
  }
  trait Parser[E, T, F <: {val pE: PackratParser[E]; val pT: PackratParser[T]}] {
    lazy val pE: FullIsoAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT
      List(
        "fold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmFold(ex, ty) },
        "unfold" ~> ("[" ~> t <~ "]") ~ e ^^ { case ty ~ ex => alg.TmUnfold(ex, ty) }
      ).reduce((a, b) => a ||| b)
    }
  }
}

trait FullIsoRecLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends FullErrorLNG[E, T, L] with RcdSubBot.Lexer with FullIsoRec.Lexer {
  val pRcdSubBotT = new RcdSubBot.Parser[T, L](){}
  val pFullIsoRecET = new FullIsoRec.Parser[E, T, L](){}
  val pFullIsoRecLNGE = pFullErrorLNGE | pFullIsoRecET.pE
  val pFullIsoRecLNGT = pFullErrorLNGT | pRcdSubBotT.pT
}

object TestFullIsoRecLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends FullIsoRecLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pFullIsoRecLNGE(Test)(l), pFullIsoRecLNGT(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get"
    ).foreach(Test.parse)
  }
}