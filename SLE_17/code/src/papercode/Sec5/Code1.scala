package papercode.Sec5

import papercode.Common


object Code1 extends Common {
  trait Alg[E] {
    def varE(x: String): E

    def lam(x: String, e: E): E
  }

  trait ExtAlg[E, T] extends Alg[E] {
    def tyInt(): T

    def tyLam(x: String, t: T, e: E): E
  }

  trait Print extends ExtAlg[String, String] {
    override def varE(x: String) = x

    override def lam(x: String, e: String) = "\\" + x + "." + e

    override def tyInt() = "Int"

    override def tyLam(x: String, t: String, e: String) = "\\" + x + ":" + t + "." + e
  }

  trait BaseParser[E] {
    lexical.delimiters += ("\\", ".")
    val alg: Alg[E]
    val pVar: Parser[E] = ident ^^ alg.varE
    lazy val pLam: Parser[E] = ("\\" ~> ident) ~ ("." ~> pE) ^^ { case x ~ e => alg.lam(x, e) }
    val pE: Parser[E] = pLam ||| pVar
  }

  trait ExtParser[E, T] extends BaseParser[E] {
    lexical.delimiters += ("\\", ":", ".")
    lexical.reserved += "Int"
    override val alg: ExtAlg[E, T]
    override lazy val pLam: Parser[E] = ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t ~ e => alg.tyLam(x, t, e) }
    val pT: Parser[T] = "Int" ^^^ alg.tyInt()
  }

  trait NewAlg[E, T] extends ExtAlg[E, T] {
    def num(x: Int): E
  }

  trait NewPrint extends NewAlg[String, String] with Print {
    def num(x: Int): String = x.toString
  }

  trait NewParser[E, T] extends ExtParser[E, T] {
    override val alg: NewAlg[E, T]
    override lazy val pLam: Parser[E] = new BaseParser[E] {
      override val alg = NewParser.this.alg
      override val pE: Parser[E] = NewParser.this.pE
    }.pLam
    val pNum: Parser[E] = numericLit ^^ { x => alg.num(x.toInt) }
    override val pE: Parser[E] = pLam ||| pVar ||| pNum
  }

  def test2(): Unit = {
    val p = new NewParser[String, String] {
      override val alg = new NewPrint {}
    }.pE
    println(parse(p)("\\x.3"))
  }

  def main(args: Array[String]): Unit = {
    test2()
  }

/*
//BEGIN_BASEPARSER_UNTYPEDLAM
trait BaseParser[E] {
  ...
  val alg: ...
  val pLam: Parser[E] =
    ("\\" ~> ident) ~ ("." ~> pE) ^^ ...
  val pE: Parser[E] = ...
}
//END_BASEPARSER_UNTYPEDLAM

//BEGIN_EXTPARSER_TYPEDLAM
trait ExtParser[E, T] extends BaseParser[E] {
  ...
  override val alg: ...
  override val pLam: Parser[E] =
    ("\\" ~> ident) ~ (":" ~> pT) ~ ("." ~> pE) ^^ ..
  val pT: Parser[T] = ...
}
//END_EXTPARSER_TYPEDLAM

//BEGIN_NEWPARSER_UNTYPEDLAM
trait NewParser[E, T] extends ExtParser[E, T] {
  ...
  override val alg: ...
  override lazy val pLam: Parser[E] = new BaseParser[E] {
    override val alg = NewParser.this.alg
    override val pE: Parser[E] = NewParser.this.pE
  }.pLam
  ...
}
//END_NEWPARSER_UNTYPEDLAM
 */

}


object Code2 extends Common {

/*
//BEGIN_LANGUAGE_COMPONENTS_VAREXPR
object VarExpr {
  trait Alg[E] {    // Abstract syntax
    def lit(n: Int): E
    ...
  }
  trait Parse[E] { ... } // Parser
  trait Print extends Alg[String] {
    ... // Pretty-printer
  }
}
//END_LANGUAGE_COMPONENTS_VAREXPR

//BEGIN_LANGUAGE_COMPONENTS_TYPEDLAM
object TypedLam {
  trait Alg[E, T] {    // Abstract syntax
    def intT(): T
    ...
  }
  trait Parse[E, T] { ... } // Parser
  trait Print extends Alg[String, String] {
    ... // Pretty-printer
  }
}
//END_LANGUAGE_COMPONENTS_TYPEDLAM

//BEGIN_LANGUAGE_COMPONENTS_VARLAMEXPR
object VarLamExpr {
  trait Alg[E, T] extends VarExpr.Alg[E]
    with TypedLam.Alg[E, T]
  trait Parse[E, T] extends VarExpr.Parse[E]
    with TypedLam.Parse[E, T] {
    override val alg: Alg[E, T]
    override val pE: Parser[E] = ...
    ...
  }
  trait Print extends VarExpr.Print
    with TypedLam.Print
}
//END_LANGUAGE_COMPONENTS_VARLAMEXPR
*/

}