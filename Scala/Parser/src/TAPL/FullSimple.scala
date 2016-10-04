package TAPL

/* <6> */
object FullSimple {
  import Util._
  trait FullSimpleAlg[E, T] {
    def unit(): E
    def as(e: E, t: T): E
    def tag(x: String, e: E, t: T): E
    def fix(e: E): E
    def caseS(e: E, l: List[(String, String, E)]): E
    def unitT(): T
    def stringT(): T
    def idT(x: String): T
    def variant(l: List[(String, T)]): T
  }
  trait Lexer extends FullSimpleAlg[String, String] {
    lexical.reserved += ("unit", "Unit", "as", "fix", "case", "of", "String")
    lexical.delimiters += ("(", ")", "<", ">", "=", ":", ",", "|", "=>")
    def unit() = "unit"
    def as(e: String, t: String) = "(" + e + ") as " + t
    def tag(x: String, e: String, t: String) = "<" + x + "=" + e + "> as " + t
    def fix(e: String) = "fix (" + e + ")"
    def caseS(e: String, l: List[(String, String, String)]) = "[case " + e + " of " + l.map(x => "<" + x._1 + "=" + x._2 + "> => " + x._3).reduce((x, y) => x + " | " + y) + "]"
    def unitT() = "Unit"
    def stringT() = "String"
    def idT(x: String) = x
    def variant(l: List[(String, String)]) = "<" + l.map(x => x._1 + ":" + x._2).reduce((x, y) => x + ", " + y) + ">"
  }
  trait Parser[E, T, F <: {val pE: PackratParser[E]; val pT: PackratParser[T]}] {
    lazy val pE: FullSimpleAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT
      "unit" ^^ { _ => alg.unit() } |||
        e ~ ("as" ~> t) ^^ { case e0 ~ t0 => alg.as(e0, t0) } |||
        ("<" ~> lcid) ~ ("=" ~> e <~ ">") ~ ("as" ~> t) ^^ { case x ~ e0 ~ t0 => alg.tag(x, e0, t0) } |||
        "fix" ~> e ^^ alg.fix |||
        ("case" ~> e <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> e) ^^ { case x1 ~ x2 ~ e0 => (x1, x2, e0) }, "|") ^^ { case e0 ~ l => alg.caseS(e0, l) } |||
        "(" ~> e <~ ")"
    }
    lazy val pT: FullSimpleAlg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Unit" ^^ { _ => alg.unitT() } |||
        "String" ^^ { _ => alg.stringT() } |||
        ucid ^^ alg.idT |||
        "<" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ alg.variant
    }
  }
}

trait FullSimpleLNG[E, T, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]}] extends ArithLNG[E, L] with FullUntyped.Lexer with TyArith.Lexer with SimpleBoolLNG[E, T, L] with FullSimple.Lexer {
  override def trueV() = super.trueV() // explicitly resolving conflicts
  override def falseV() = super.falseV()
  override def ifS(e1 : String, e2 : String, e3 : String) = super.ifS(e1, e2, e3)
  override def bool() = super.bool()
  val pFullUntypedE = new FullUntyped.Parser[E, L](){}
  val pTyArithT = new TyArith.Parser[T, L](){}
  val pFullSimpleET = new FullSimple.Parser[E, T, L](){}
  val pFullSimpleLNGE = pArithLNGE | pFullUntypedE.pE | pSimpleBoolLNGE | pFullSimpleET.pE
  val pFullSimpleLNGT = pTyArithT.pT | pSimpleBoolET.pT | pFullSimpleET.pT
}

object TestFullSimpleLNG {
  import Util._
  class List[E, T](pe : PackratParser[E], pt : PackratParser[T]) { val pE = pe; val pT = pt }
  object Test extends FullSimpleLNG[String, String, List[String, String]] {
    lazy val parser : (=> List[String, String]) => List[String, String] = l =>
      new List[String, String](pFullSimpleLNGE(Test)(l), pFullSimpleLNGT(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)",
      "\\f:T. \\x:Nat. f (f x)",
      "<l=unit> as <l:Unit, r:Unit>",
      "\\a:Unit.fix (\\x:T.x)",
      "case a of <phy=x> => x.first | <vir=y> => y.name"
    ).foreach(Test.parse)
  }
}