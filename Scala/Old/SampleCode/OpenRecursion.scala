package Test

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object OpenRecursion extends StandardTokenParsers with PackratParsers {
  
  class Expr {
    override val toString = ""
  }
  
  class Var(x : String) extends Expr {
    override val toString = x
  }
  val pVar : (=> PackratParser[Expr]) => PackratParser[Expr] = p =>
    ident ^^ { x => new Var(x) }
  
  class App(e1 : Expr, e2 : Expr) extends Expr {
    override val toString = e1.toString + " " + e2.toString
  }
  val pApp : (=> PackratParser[Expr]) => PackratParser[Expr] = p =>
    p ~ p ^^ { case e1 ~ e2 => new App(e1, e2) }

  class Type {
    override val toString = ""
  }
  
  class Int extends Type {
    override val toString = "Int"
  }
  val pInt : (=> (PackratParser[Expr], PackratParser[Type])) => PackratParser[Type] = p =>
    "Int" ^^ { _ => new Int() }
    
  class Lam(x : String, t : Type, e : Expr) extends Expr {
    override val toString = "\\" + x + ":" + t.toString + "." + e.toString
  }
  val pLam : (=> (PackratParser[Expr], PackratParser[Type])) => PackratParser[Expr] = p => {
    val pE = p._1
    val pT = p._2
    ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t ~ e => new Lam(x, t, e) }
  }
  
  val p : (=> (PackratParser[Expr], PackratParser[Type])) => (PackratParser[Expr], PackratParser[Type])
    = p => (pVar(p._1) | pApp(p._1) | pLam(p), pInt(p))
  
  lexical.reserved += ("Int")
  lexical.delimiters += ("\\", ":", ".")
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
    runParser(fix(p)._1)("\\x:Int.x x")
  }

}
