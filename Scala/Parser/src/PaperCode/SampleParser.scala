package PaperCode

import TAPL.Util._

object SampleParser {

//BEGIN_PACKRAT_EXAMPLE
lexical.reserved += ("str")
lexical.delimiters += ("(", ")")

val p : PackratParser[String] =
  "str" ~> ("(" ~> numericLit <~ ")") ^^ { x => x.toString }
//END_PACKRAT_EXAMPLE

//BEGIN_PACKRAT_RUNPARSER
def runParser(p: Parser[_]): String => Unit = in => {
  val t = phrase(p)(new lexical.Scanner(in))
  if (t.successful) println(t.get) else scala.sys.error(t.toString)
}
//END_PACKRAT_RUNPARSER  
     
}

object OpenRecursion {

//BEGIN_OPENRECURSION_FIX
def fix[A](f : (=> A) => A) : A = {
  lazy val a : A = f(a)
  a
}
//END_OPENRECURSION_FIX

}