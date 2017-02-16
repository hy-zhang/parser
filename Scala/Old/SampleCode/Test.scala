package Test

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Test extends StandardTokenParsers with PackratParsers {
  
  class Expr {
    override val toString = ""
  }
  
  class Var(x : String) extends Expr {override val toString = x}
  val pVar : (=> PackratParser[Expr]) => PackratParser[Expr] =
    p => ident ^^ { x => new Var(x) }
  
  class App(e1 : Expr, e2 : Expr) extends Expr {
    override val toString = "[" + e1.toString + " " + e2.toString + "]"
  }
  val pApp : (=> PackratParser[Expr]) => PackratParser[Expr] =
    p => p ~ p ^^ { case e1 ~ e2 => new App(e1, e2) }
    
  val pVarApp : (=> PackratParser[Expr]) => PackratParser[Expr] =
    p => pApp(p) | pVar(p) 

  def runParser(p : Parser[_]) : String => Unit = in => {
    phrase(p)(new lexical.Scanner(in)) match {
      case t if t.successful => println(t.get.toString())
      case t                 => scala.sys.error(t.toString)
    }
  }
  
  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }
  
  def merge[E, F, G, H[_,_]](op : F => G => H[F, G], x : (=> E) => F, y : (=> E) => G)
    : (=> E) => H[F, G] = e => op(x(e))(y(e))
  
  type Parser[E] = PackratParser[E]
  type GenParser[E] = (=> Parser[E]) => Parser[E]
  implicit class ParserCombinator[E, R](x : (=> R) => Parser[E]) {
    type H[A, B] = Parser[E]
    def |||(y : (=> R) => Parser[E]) : (=> R) => Parser[E] = {
      val alt : Parser[E] => Parser[E] => Parser[E] = a => b => a ||| b
      merge[R, Parser[E], Parser[E], H](alt, x, y)
    }
  }
  
  type Pair = (Parser[Expr], Parser[Type])
  
  class Type
  class IntT extends Type
  val pInt : (=> Pair) => Parser[Type] =
    p => "Int" ^^ { _ => new IntT() }

  class Lam(x : String, t : Type, e : Expr) extends Expr
  val pLam : (=> Pair) => Parser[Expr] =
    p => ("\\" ~> ident) ~ (":" ~> p._2) ~ ("." ~> p._1) ^^
      { case x ~ t ~ e => new Lam(x, t, e) }
    
  
  val pVar2 : (=> Pair) => Parser[Expr] = p => pVar(p._1)
  val pApp2 : (=> Pair) => Parser[Expr] = p => pApp(p._1)

  val pET : (=> Pair) => Pair = {
    val pE = pVar2 ||| pApp2 ||| pLam
    val pT = pInt
    p => (pE(p), pT(p))
  }
    
  
  
  def main(args : Array[String]) = {
    runParser(fix(pVarApp))("x y")
  }

}
