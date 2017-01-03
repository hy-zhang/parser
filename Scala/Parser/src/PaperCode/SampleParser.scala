package PaperCode

import TAPL.Util._

/*
//BEGIN_LANGUAGE_COMPONENTS_VAREXPR
object VarExpr {
  // Abstract syntax
  trait Alg[E] {
    def lit(n: Int): E
    def add(e1: E, e2: E): E
    def varE(x: String): E
  }
  
  // Parser
  trait Parser[E] { ... }
  
  // Pretty-printer
  trait Print extends Alg[String] {
    ...
  }
}
//END_LANGUAGE_COMPONENTS_VAREXPR

//BEGIN_LANGUAGE_COMPONENTS_TYPEDLAM
object TypedLam {
  // Abstract syntax
  trait Alg[E, T] {
    def intT(): T
    def arrowT(t1: T, t2: T): T
    def lam(x: String, t: T, e: E): E
  }
  
  // Parser
  trait Parser[E] { ... }
  
  // Pretty-printer
  trait Print extends Alg[String, String] {
    ...
  }
}
//END_LANGUAGE_COMPONENTS_TYPEDLAM

//BEGIN_LANGUAGE_COMPONENTS_VARLAMEXPR
object VarLamExpr {
  trait Alg[E, T] extends VarExpr.Alg[E] with TypedLam.Alg[E, T]
  
  trait Parser[E, T] extends VarExpr.Parser[E] with TypedLam.Parser[E, T] {
    override val alg: Alg[E, T]
    override val pE = ...
  }
  
  trait Print extends VarExpr.Print with TypedLam.Print
}
//END_LANGUAGE_COMPONENTS_VARLAMEXPR
/*

object SampleParser {

//BEGIN_PACKRAT_EXAMPLE
lexical.reserved += "str"
lexical.delimiters += ("(", ")")

val p: PackratParser[Int] =
  "str" ~> ("(" ~> numericLit <~ ")") ^^ { x => x.toInt }
//END_PACKRAT_EXAMPLE
     
}