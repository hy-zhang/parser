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
  
  def main(args : Array[String]) = {
    runParser(fix(pVarApp))("x y")
  }

}
