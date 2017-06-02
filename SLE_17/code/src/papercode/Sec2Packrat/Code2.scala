package papercode.Sec2Packrat


object Code2 extends Code1 {
//BEGIN_PACKRAT_RUNPARSER
val p = new AParser {}
val r = parse(p.pExpr)("1 + 2").print // "(1+2)"
//END_PACKRAT_RUNPARSER

  def main(args: Array[String]): Unit = {
    println(r)
  }
}

//BEGIN_PACKRAT_PAPERCODE
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.PackratParsers

object Code extends StandardTokenParsers
  with PackratParsers {
  type Parser[E] = PackratParser[E]
  def parse[E](p: Parser[E]): String => E = in => {
    val t = phrase(p)(new lexical.Scanner(in))
    t.getOrElse(sys.error(t.toString))
  }
  // Any Scala code in the paper comes here
}
//END_PACKRAT_PAPERCODE