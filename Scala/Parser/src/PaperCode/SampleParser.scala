package PaperCode

import TAPL.Util._

object SampleParser {

//BEGIN_PACKRAT_EXAMPLE
lexical.reserved += "str"
lexical.delimiters += ("(", ")")

val p: PackratParser[String] =
  "str" ~> ("(" ~> numericLit <~ ")") ^^ { x => x.toString }
//END_PACKRAT_EXAMPLE

//BEGIN_PACKRAT_RUNPARSER
def parse[E](p: Parser[E]): String => E = in => {
  val t = phrase(p)(new lexical.Scanner(in))
  if (t.successful) t.get else scala.sys.error(t.toString)
}
//END_PACKRAT_RUNPARSER  
     
}

object OpenRecursion {

//BEGIN_OPENRECURSION_FIX
def fix[T](f: Open[T]): T = { lazy val a: T = f(a); a }
//END_OPENRECURSION_FIX

//BEGIN_OPENRECURSION_FIB
def fib: Int => Int = {
  case 0 => 0
  case 1 => 1
  case n => fib(n - 1) + fib(n - 2)
}

fib(2)  // 1
//END_OPENRECURSION_FIB

//BEGIN_OPENRECURSION_FIB2
type Open[T] = (=> T) => T

def fib2: Open[Int => Int] = self => {
  case 0 => 0
  case 1 => 1
  case n => self.apply(n - 1) + self.apply(n - 2)
}
//END_OPENRECURSION_FIB2

//BEGIN_OPENRECURSION_FIB_FIX
fix(fib2)(2)  // 1
//END_OPENRECURSION_FIB_FIX

//BEGIN_OPENRECURSION_COMPOSE
implicit class Compose[A, B, C](f: Open[A => B]) {
  def *(g: Open[A => C]): Open[A => (B, C)] = self => x =>
    (f(self.apply(_)._1)(x), g(self.apply(_)._2)(x))
}
//END_OPENRECURSION_COMPOSE

//BEGIN_OPENRECURSION_SHOW
def show: Open[Int => String] = self => x => x.toString
//END_OPENRECURSION_SHOW

//BEGIN_OPENRECURSION_MERGE
def merge[E, F, G, H[_, _]](op: F => G => H[F, G], x: (=> E) => F, y: (=> E) => G)
  : (=> E) => H[F, G] = e => op(x(e))(y(e))
//END_OPENRECURSION_MERGE
}