package TAPL

import Util._

/* <6> */
object TypedRecord {

  trait Alg[E, T] extends Record.Alg[E] {
    def TyRecord(l: List[(String, T)]): T
  }

  trait Print extends Alg[String, String] with Record.Print {
    def TyRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends Record.Parser[E, F] {
    lexical.delimiters += (":", ",", "{", "}")

    val pTypedRecordE: Alg[E, T] => (=> F) => PackratParser[E] = pRecordE

    val pTypedRecordT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord
    }
  }

}

object TypeVar {

  trait Alg[T] {
    def TyVar(x: String): T
  }

  trait Print extends Alg[String] {
    def TyVar(x: String) = x
  }

  trait Parser[T, F <: {val pT : PackratParser[T]}] {
    val pTypeVarT: Alg[T] => (=> F) => PackratParser[T] = alg => l => ucid ^^ alg.TyVar
  }

}

object Variant {

  trait Alg[E, T] {
    def TmTag(x: String, e: E, t: T): E

    def TmCase(e: E, l: List[(String, String, E)]): E

    def TyVariant(l: List[(String, T)]): T
  }

  trait Print extends Alg[String, String] {
    def TmTag(x: String, e: String, t: String) = "<" + x + "=" + e + "> as " + t

    def TmCase(e: String, l: List[(String, String, String)]) = "[case " + e + " of " + l.map(x => "<" + x._1 + "=" + x._2 + "> => " + x._3).reduce((x, y) => x + " | " + y) + "]"

    def TyVariant(l: List[(String, String)]) = "<" + l.map(x => x._1 + ":" + x._2).reduce((x, y) => x + ", " + y) + ">"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("as", "case", "of")
    lexical.delimiters += ("<", ">", "=", ":", ",", "|", "=>")

    val pVariantE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      ("<" ~> lcid) ~ ("=" ~> e <~ ">") ~ ("as" ~> t) ^^ { case x ~ e0 ~ t0 => alg.TmTag(x, e0, t0) } |||
        ("case" ~> e <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> e) ^^ { case x1 ~ x2 ~ e0 => (x1, x2, e0) }, "|") ^^ { case e0 ~ l => alg.TmCase(e0, l) }
    }

    val pVariantT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      "<" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ alg.TyVariant
    }
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

    def TmAscribe(e: String, t: String) = "(" + e + ") as " + t

    def TmFix(e: String) = "fix (" + e + ")"

    def TmInert(t: String) = "inert [" + t + "]"

    def TyUnit() = "Unit"

    def TyString() = "String"

    def TyFloat() = "Float"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] {
    lexical.reserved += ("unit", "Unit", "as", "fix", "String", "Float", "inert")
    lexical.delimiters += ("(", ")", "[", "]")

    val pExtensionE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      "unit" ^^ { _ => alg.TmUnit() } |||
        e ~ ("as" ~> t) ^^ { case e0 ~ t0 => alg.TmAscribe(e0, t0) } |||
        "fix" ~> e ^^ alg.TmFix |||
        "inert" ~> "[" ~> t <~ "]" ^^ alg.TmInert |||
        "(" ~> e <~ ")"
    }

    val pExtensionT: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
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

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends TyArith.Parser[E, T, L] with Typed.Parser[E, T, L] with FloatString.Parser[E, L] with Let.Parser[E, L]
      with TypedRecord.Parser[E, T, L] with Extension.Parser[E, T, L] with TypeVar.Parser[T, L] {
    val pSimpleE = pTyArithE | pTypedE | pTypedRecordE | pExtensionE | pFloatStringE | pLetE
    val pSimpleT = pTyArithT | pTypedT | pTypedRecordT | pExtensionT | pTypeVarT
  }

}

object FullSimple {

  trait Alg[E, T] extends Simple.Alg[E, T] with Variant.Alg[E, T]

  trait Print extends Alg[String, String] with Simple.Print with Variant.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends Simple.Parser[E, T, L] with Variant.Parser[E, T, L] {
    val pFullSimpleE = pSimpleE | pVariantE
    val pFullSimpleT = pSimpleT | pVariantT
  }

}

object TestFullSimple {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parse[E, T](inp: String)(alg: FullSimple.Alg[E, T]) = {
    def parser(l: => List[E, T]): List[E, T] = {
      val lang = new FullSimple.Parser[E, T, List[E, T]] {}
      new List[E, T](lang.pFullSimpleE(alg)(l), lang.pFullSimpleT(alg)(l))
    }
    runParser(fix(parser).pE)(inp)
  }

  def parseAndPrint(inp: String) = parse(inp)(new FullSimple.Print {})

  def main(args: Array[String]) = {
    List(
      "(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)",
      "\\f:T. \\x:Nat. f (f x)",
      "<l=unit> as <l:Unit, r:Unit>",
      "\\a:Unit.fix (\\x:T.x)",
      "case a of <phy=x> => x.first | <vir=y> => y.name",
      "succ (pred 0)",
      "iszero (pred (succ (succ 0)))",
      "inert [Bool->Nat]",
      "let x = false in \\y:Bool->Nat. y x"
    ).foreach(parseAndPrint)
  }
}