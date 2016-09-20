package TAPL

/* <6> */
object FullSimple {
  import Util._
  trait Lexer {
    lexical.reserved += ("unit", "Unit", "as", "fix", "case", "of", "String")
    lexical.delimiters += ("(", ")", "<", ">", "=", ":", ",", "|", "=>")
  }
  
  type ParserT[E, T] = {val pE : PackratParser[E]; val pT : PackratParser[T]}
  
  trait FullSimpleAlg[E, T] {
    def unit() : E
    def as(e : E, t : T) : E
    def tag(x : String, e : E, t : T) : E
    def fix(e : E) : E
    def caseS(e : E, l : List[(String, String, E)]) : E
    def unitT() : T
    def stringT() : T
    def idT(x : String) : T
    def variant(l : List[(String, T)]) : T
  }
  
  trait Pretty extends FullSimpleAlg[String, String] {
    def unit() = "unit"
    def as(e : String, t : String) = "(" + e + ") as " + t
    def tag(x : String, e : String, t : String) = "<" + x + "=" + e + "> as " + t
    def fix(e : String) = "fix (" + e + ")"
    def caseS(e : String, l : List[(String, String, String)]) = "[case " + e + " of " + l.map(x => "<" + x._1 + "=" + x._2 + "> => " + x._3).reduce((x, y) => x + " | " + y) + "]"
    def unitT() = "Unit"
    def stringT() = "String"
    def idT(x : String) = x
    def variant(l : List[(String, String)]) = "<" + l.map(x => x._1 + ":" + x._2).reduce((x, y) => x + ", " + y) + ">"
  }
  
  trait Parser[E, T, F <: ParserT[E, T]] {
    lazy val pE : FullSimpleAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT
      "unit" ^^ { _ => alg.unit() } |||
      e ~ ("as" ~> t) ^^ { case e0 ~ t0 => alg.as(e0, t0) } |||
      ("<" ~> lcid) ~ ("=" ~> e <~ ">") ~ ("as" ~> t) ^^ { case x ~ e0 ~ t0 => alg.tag(x, e0, t0) } |||
      "fix" ~> e ^^ alg.fix |||
      ("case" ~> e <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> e) ^^ { case x1 ~ x2 ~ e0 => (x1, x2, e0) }, "|") ^^ { case e0 ~ l => alg.caseS(e0, l)} |||
      "(" ~> e <~ ")"
    }
    lazy val pT : FullSimpleAlg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      "Unit" ^^ { _ => alg.unitT() } |||
      "String" ^^ { _ => alg.stringT() } |||
      ucid ^^ alg.idT |||
      "<" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ alg.variant
    }
  }
  def pET[F <: ParserT[String, String]] = {
    (new Parser[String, String, F](){}.pE(new Pretty(){}),
     new Parser[String, String, F](){}.pT(new Pretty(){}))
  }
}

object TestFullSimple extends Arith.Lexer with FullUntyped.Lexer
    with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer {
  import Util._
  trait List[E, T] { val pE : PackratParser[E]; val pT : PackratParser[T] }
  type L = List[String, String]
  
  trait Parse[E, T] {
    type L = List[E, T]
    type PE = (=> L) => PackratParser[E]
    type PT = (=> L) => PackratParser[T]
    val pArithE : PE
    val pFullUntypedE : PE
    val pTyArithT : PT
    val pSimpleBoolET : (PE, PT)
    val pFullSimpleET : (PE, PT)
    val p : (=> L) => L = l => new L() {
      override lazy val pE = List(pArithE, pFullUntypedE, pSimpleBoolET._1, pFullSimpleET._1).reduce(alt[E, L])(l)
      override lazy val pT = List(pTyArithT, pSimpleBoolET._2, pFullSimpleET._2).reduce(alt[T, L])(l)
    }
  }
  
  lazy val parser = new Parse[String, String]() {
    lazy val pArithE = Arith.pE[L]
    lazy val pFullUntypedE = FullUntyped.pE[L]
    lazy val pTyArithT = TyArith.pT[L]
    lazy val pSimpleBoolET = SimpleBool.pET[L]
    lazy val pFullSimpleET = FullSimple.pET[L]
  }
  lazy val parse = runParser(fix(parser.p).pE)
 
  def main(args : Array[String]) = {
    parse("(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)")
    parse("\\f:T. \\x:Nat. f (f x)")
    parse("<l=unit> as <l:Unit, r:Unit>")
    parse("\\a:Unit.fix (\\x:T.x)")
    parse("case a of <phy=x> => x.first | <vir=y> => y.name")
  }
}