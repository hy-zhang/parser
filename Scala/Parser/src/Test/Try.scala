package Test

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

object Try extends RegexParsers with PackratParsers {
  
  def fix[A](f: (=> A) => A): A = { lazy val a: A = f(a); a }
  
  trait List[E] {
    val pE : PackratParser[Any]
  }
  
  trait MyParser[E] {
    val a : Int => (=> List[E]) => PackratParser[Any] = i => p => {
      lazy val e = p.pE
      e ~ e | "x" | "y"
    }
  }
  
  trait Test[E] {
    val p : (=> List[E]) => List[E] = l => new List[E]() {
      override val pE = new MyParser[E](){}.a(0)(l)
    }
  }
  
  
  
  def input(s: String) = parseAll(fix(new Test[Any](){}.p).pE, s) match {
    case t if t.successful => t.get
    case t                 => scala.sys.error(t.toString)
  }
  
  def main(args : Array[String]) {
    println(input("x y x"))
  }
  
}