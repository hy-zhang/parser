package Test

import scala.util.parsing.combinator._

object OA extends RegexParsers {
    def fix[T, R](f: (T => R) => (T => R)): (T => R) = new Function1[T, R] {
        def apply(t: T): R = f(this)(t)
    }
    
    def fix[A](f: (=> A) => A): A = {
        lazy val a: A = f(a)
        a
    }
  
    trait BoolAlg[E] {
        def trueV() : E
        def falseV() : E
        def ifS(e1 : E, e2 : E, e3 : E) : E
    }
    
    def parseBool[E](alg : BoolAlg[E]) : (=> Parser[E]) => Parser[E] = p => {
        "true" ^^ { _ => alg.trueV() } |
        "false" ^^ { _ => alg.falseV() } |
        ("if " ~> p) ~ (" then " ~> p) ~ (" else " ~> p) ^^ { case e1 ~ e2 ~ e3 => alg.ifS(e1, e2, e3) }
    }
    
    val pretty : BoolAlg[String] = new BoolAlg[String]() {
        def trueV() = "TRUE"
        def falseV() = "FALSE"
        def ifS(e1 : String, e2 : String, e3 : String) = ""
    }
    
    def parse(in : String) : String = {
        lazy val p = fix(parseBool(pretty))
        parseAll(p, in) match {
            case Success(res, _) => res
            case failure : NoSuccess => scala.sys.error(failure.msg)
        }
    }
      
    def main(args : Array[String]) = {
    	  println(parse("true"))
    }
}