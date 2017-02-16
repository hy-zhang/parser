package TAPL

import TAPL.Lib._


object TypedRecord {

  trait Alg[E, T] extends Record.Alg[E] {
    def TyRecord(l: List[(String, T)]): T
  }

  trait Print extends Alg[String, String] with Record.Print {
    def TyRecord(l: List[(String, String)]): String =
      "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }

  trait Parse[E, T] extends ETParser[E, T] with Record.Parse[E] {
    lexical.delimiters += (":", ",", "{", "}")

    override val alg: Alg[E, T]

    val pTypedRecordE: Parser[E] = pRecordE

    val pTypedRecordT: Parser[T] =
      "{" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord

  }

}

object TypeVar {

  trait Alg[T] {
    def TyVar(x: String): T
  }

  trait Print extends Alg[String] {
    def TyVar(x: String): String = x
  }

  trait Parse[T] {
    val alg: Alg[T]

    val pTypeVarT: Parser[T] = ucid ^^ alg.TyVar
  }

}

object Variant {

  trait Alg[E, T] {
    def TmTag(x: String, e: E, t: T): E

    def TmCase(e: E, l: List[(String, String, E)]): E

    def TyVariant(l: List[(String, T)]): T
  }

  trait Print extends Alg[String, String] {
    def TmTag(x: String, e: String, t: String): String = "<" + x + "=" + e + "> as " + t

    def TmCase(e: String, l: List[(String, String, String)]): String =
      "[case " + e + " of " + l.map(x => "<" + x._1 + "=" + x._2 + "> => " + x._3).reduce((x, y) => x + " | " + y) + "]"

    def TyVariant(l: List[(String, String)]): String =
      "<" + l.map(x => x._1 + ":" + x._2).reduce((x, y) => x + ", " + y) + ">"
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("as", "case", "of")
    lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

    val alg: Alg[E, T]

    val pVariantE: Parser[E] = {
      ("<" ~> lcid) ~ ("=" ~> pE <~ ">") ~ ("as" ~> pT) ^^ { case x ~ e0 ~ t0 => alg.TmTag(x, e0, t0) } |||
        ("case" ~> pE <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> pE) ^^ { case x1 ~ x2 ~ e0 =>
          (x1, x2, e0)
        }, "|") ^^ { case e0 ~ l => alg.TmCase(e0, l) }
    }

    val pVariantT: Parser[T] =
      "<" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ alg.TyVariant
  }

}

object Extension {

  trait Alg[E, T] {
    def TmUnit(): E

    def TmAscribe(e: E, t: T): E

    def TmFix(e: E): E

    def TmInert(t: T): E

    def TyUnit(): T

    def TyString(): T

    def TyFloat(): T
  }

  trait Print extends Alg[String, String] {
    def TmUnit() = "unit"

    def TmAscribe(e: String, t: String): String = "(" + e + ") as " + t

    def TmFix(e: String): String = "fix (" + e + ")"

    def TmInert(t: String): String = "inert [" + t + "]"

    def TyUnit() = "Unit"

    def TyString() = "String"

    def TyFloat() = "Float"
  }

  trait Parse[E, T] extends ETParser[E, T] {
    lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
    lexical.delimiters += ("(", ")", "[", "]")

    val alg: Alg[E, T]

    val pExtensionE: Parser[E] = {
      "unit" ^^ { _ => alg.TmUnit() } |||
        pE ~ ("as" ~> pT) ^^ { case e0 ~ t0 => alg.TmAscribe(e0, t0) } |||
        "fix" ~> pE ^^ alg.TmFix |||
        "inert" ~> "[" ~> pT <~ "]" ^^ alg.TmInert
    }

    val pExtensionT: Parser[T] = {
      "Unit" ^^ { _ => alg.TyUnit() } |||
        "String" ^^ { _ => alg.TyString() } |||
        "Float" ^^ { _ => alg.TyFloat() }
    }
  }

}

// todo: rename; FullSimple - Variant
object Simple {

  trait Alg[E, T] extends TyArith.Alg[E, T] with Typed.Alg[E, T] with FloatString.Alg[E] with Let.Alg[E]
    with TypedRecord.Alg[E, T] with Extension.Alg[E, T] with TypeVar.Alg[T]

  trait Print extends Alg[String, String] with TyArith.Print with Typed.Print with FloatString.Print
    with Let.Print with TypedRecord.Print with Extension.Print with TypeVar.Print

  trait Parse[E, T] extends TyArith.Parse[E, T] with Typed.Parse[E, T] with FloatString.Parse[E]
    with Let.Parse[E] with TypedRecord.Parse[E, T] with Extension.Parse[E, T] with TypeVar.Parse[T] {

    override val alg: Alg[E, T]

    val pSimpleE: Parser[E] =
      pTyArithE ||| pTypedE ||| pTypedRecordE ||| pExtensionE ||| pFloatStringE ||| pLetE

    val pSimpleT: Parser[T] =
      pTyArithT ||| pTypedT ||| pTypedRecordT ||| pExtensionT ||| pTypeVarT
  }

}

object FullSimple {

  trait Alg[E, T] extends Simple.Alg[E, T] with Variant.Alg[E, T]

  trait Print extends Alg[String, String] with Simple.Print with Variant.Print

  trait Parse[E, T] extends Simple.Parse[E, T] with Variant.Parse[E, T] {
    override val alg: Alg[E, T]

    val pFullSimpleE: Parser[E] = pSimpleE ||| pVariantE
    val pFullSimpleT: Parser[T] = pSimpleT ||| pVariantT

    override val pE: Parser[E] = pFullSimpleE

    override val pT: Parser[T] = pFullSimpleT
  }

}

object TestFullSimple {

  def parseWithAlg[E, T](inp: String)(a: FullSimple.Alg[E, T]): E = {
    val p = new FullSimple.Parse[E, T] {
      override val alg: FullSimple.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new FullSimple.Print {}))

}