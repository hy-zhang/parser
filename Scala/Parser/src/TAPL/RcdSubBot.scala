package TAPL

/* <9> */
object RcdSubBot {
  import Util._
  trait RcdSubBotAlg[T] {
    def TyRecord(l: List[(String, T)]): T
  }
  trait Lexer extends RcdSubBotAlg[String] {
    lexical.delimiters += ("{", "}", ",", ":")
    def TyRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }
  trait Parser[T, F <: {val pT: PackratParser[T]}] {
    lazy val pT: RcdSubBotAlg[T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord
    }
  }
}

trait RcdSubBotLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends FullUntypedLNG[E, L] with SimpleBoolLNG[E, T, L] with FullRef.Lexer with RcdSubBot.Lexer {
  override def trueV() = super.trueV()
  override def falseV() = super.falseV()
  override def ifS(e1 : String, e2 : String, e3 : String) = super.ifS(e1, e2, e3)
  override def id(x: String) = super.id(x)
  override def app(e1: String, e2: String) = super.app(e1, e2)
  val pFullRefET = new FullRef.Parser[E, T, L](){}
  val pRcdSubBotT = new RcdSubBot.Parser[T, L](){}
  val pRcdSubBotLNGE = pFullUntypedLNGE | pSimpleBoolLNGE | pFullRefET.pE
  val pRcdSubBotLNGT = pSimpleBoolLNGT | pFullRefET.pT | pRcdSubBotT.pT
}

object TestRcdSubBotLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends RcdSubBotLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pRcdSubBotLNGE(Test)(l), pRcdSubBotLNGT(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "\\x:Top.x",
      "(\\r:{x:Top->Top}. r.x r.x) {x=\\z:Top.z, y=\\z:Top.z}",
      "\\x:Bot. x x"
    ).foreach(Test.parse)
  }
}