package Test

import scala.util.parsing.combinator._

object OAParser extends RegexParsers {

  trait List[E, T] { // Pair of Expr-parser and Type-parser
    def pE() : Parser[E]
    def pT() : Parser[T]
  }
  
  def fix[A](f: (=> A) => A): A = { lazy val a: A = f(a); a }
    
  trait SimpleAlg[E, T] {
    def bool() : T // Type: Bool
    def int() : T  // Type: Int
    def zero() : E // Expr: 0
  }
  
  trait PrettySimple extends SimpleAlg[String, String] { // Pretty-printer
    def bool() = "Bool"
    def int() = "Int"
    def zero() = "0"
  }
  
  def parseSimpleE[E, T](alg : SimpleAlg[E, T]) : (=> List[E, T]) => Parser[E] = l =>
    "0" ^^ { _ => alg.zero() }
    
  def parseSimpleT[E, T](alg : SimpleAlg[E, T]) : (=> List[E, T]) => Parser[T] = l =>
    "Bool" ^^ { _ => alg.bool() } |
    "Int" ^^ { _ => alg.int() }
  
  def parseSimple[E, T](alg : SimpleAlg[E, T]) : (=> List[E, T]) => List[E, T] = l =>
    new List[E, T]() {
      def pE() = parseSimpleE(alg)(l)
      def pT() = parseSimpleT(alg)(l)
    }
  
  trait LamAlg[E, T] extends SimpleAlg[E, T] {
    def lam(t : T, e : E) : E // Expr: lambda (parameter is fixed: "x")
  }
  
  trait PrettyLam extends PrettySimple with LamAlg[String, String] { // Pretty-printer
    def lam(t : String, e : String) = "[\\(x : " + t + "). " + e + "]"
  }
  
  def parseLam[E, T](alg : LamAlg[E, T]) : (=> List[E, T]) => List[E, T] = l => {
    val p = parseSimple(alg)(l)
    new List[E, T]() {
      def pE() = (("\\x : " ~> l.pT()) ~ (". " ~> l.pE()) ^^ { case t ~ e => alg.lam(t, e) }) | p.pE()
      def pT() = failure("") | p.pT()
    }
  }
  
  def parse(in : String) : String = {
    val p = fix(parseLam(new PrettyLam(){}))
    parseAll(p.pE(), in) match {
      case Success(res, _) => res
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }
      
  def main(args : Array[String]) = {
    println(parse("\\x : Bool. \\x : Int. 0"))
  }
  
}