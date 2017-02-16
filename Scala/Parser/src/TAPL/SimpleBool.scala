package TAPL

import TAPL.Lib._


object Typed {

  trait Alg[E, T] extends VarApp.Alg[E] {
    def TmAbs(x: String, t: T, e: E): E

    def TyArr(t1: T, t2: T): T
  }

  trait Print extends Alg[String, String] with VarApp.Print {
    def TmAbs(x: String, t: String, e: String): String = "\\(" + x + ":" + t + ")." + e

    def TyArr(t1: String, t2: String): String = t1 + "->" + t2
  }

  trait Parse[E, T] extends ETParser[E, T] with VarApp.Parse[E] {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

    override val alg: Alg[E, T]

    private val pAbsE: Parser[E] =
      ("\\" ~> lcid) ~ (":" ~> pT) ~ ("." ~> pE) ^^ { case x ~ t0 ~ e0 => alg.TmAbs(x, t0, e0) }

    val pTypedE: Parser[E] = pVarAppE ||| pAbsE

    val pTypedT: Parser[T] =
      pT ~ ("->" ~> pT) ^^ { case t1 ~ t2 => alg.TyArr(t1, t2) } |||
        "(" ~> pT <~ ")"
  }

}

object SimpleBool {

  trait Alg[E, T] extends Typed.Alg[E, T] with TypedBool.Alg[E, T]

  trait Print extends Alg[String, String] with Typed.Print with TypedBool.Print

  trait Parse[E, T] extends Typed.Parse[E, T] with TypedBool.Parse[E, T] {
    override val alg: Alg[E, T]

    val pSimpleBoolE: Parser[E] = pTypedE ||| pTypedBoolE
    val pSimpleBoolT: Parser[T] = pTypedT ||| pTypedBoolT

    override val pE: Parser[E] = pSimpleBoolE
    override val pT: Parser[T] = pSimpleBoolT
  }

}

object TestSimpleBool {

  def parseWithAlg[E, T](inp: String)(a: SimpleBool.Alg[E, T]): E = {
    val p = new SimpleBool.Parse[E, T] {
      override val alg: SimpleBool.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new SimpleBool.Print {}))

}