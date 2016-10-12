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
      val t = l.pT

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

// todo: use TypedRecord and TypeVar
object FullSimpleExt {

  trait Alg[E, T] extends TypeVar.Alg[T] {
    def TmUnit(): E

    def TmAscribe(e: E, t: T): E

    def TmTag(x: String, e: E, t: T): E

    def TmFix(e: E): E

    def TmCase(e: E, l: List[(String, String, E)]): E

    def TyUnit(): T

    def TyString(): T

    def TyVariant(l: List[(String, T)]): T

    def TyFloat(): T

    def TyRecord(l: List[(String, T)]): T
  }

  trait Print extends Alg[String, String] with TypeVar.Print {
    def TmUnit() = "unit"

    def TmAscribe(e: String, t: String) = "(" + e + ") as " + t

    def TmTag(x: String, e: String, t: String) = "<" + x + "=" + e + "> as " + t

    def TmFix(e: String) = "fix (" + e + ")"

    def TmCase(e: String, l: List[(String, String, String)]) = "[case " + e + " of " + l.map(x => "<" + x._1 + "=" + x._2 + "> => " + x._3).reduce((x, y) => x + " | " + y) + "]"

    def TyUnit() = "Unit"

    def TyString() = "String"

    def TyVariant(l: List[(String, String)]) = "<" + l.map(x => x._1 + ":" + x._2).reduce((x, y) => x + ", " + y) + ">"

    def TyFloat() = "Float"

    def TyRecord(l: List[(String, String)]) = "{" + l.map(x => x._1 + ": " + x._2).reduce((x, y) => x + ", " + y) + "}"
  }

  trait Parser[E, T, F <: {val pE : PackratParser[E]; val pT : PackratParser[T]}] extends TypeVar.Parser[T, F] {
    lexical.reserved += ("unit", "Unit", "as", "fix", "case", "of", "String", "Float")
    lexical.delimiters += ("(", ")", "<", ">", "=", ":", ",", "|", "=>", "{", "}")

    val pFullSimpleExtE: Alg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      val e = l.pE
      val t = l.pT

      "unit" ^^ { _ => alg.TmUnit() } |||
        e ~ ("as" ~> t) ^^ { case e0 ~ t0 => alg.TmAscribe(e0, t0) } |||
        ("<" ~> lcid) ~ ("=" ~> e <~ ">") ~ ("as" ~> t) ^^ { case x ~ e0 ~ t0 => alg.TmTag(x, e0, t0) } |||
        "fix" ~> e ^^ alg.TmFix |||
        ("case" ~> e <~ "of") ~ repsep(("<" ~> lcid) ~ ("=" ~> lcid) ~ ((">" ~ "=>") ~> e) ^^ { case x1 ~ x2 ~ e0 => (x1, x2, e0) }, "|") ^^ { case e0 ~ l => alg.TmCase(e0, l) } |||
        "(" ~> e <~ ")"
    }

    private val pFullSimpleExtT2: Alg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      val t = l.pT

      "Unit" ^^ { _ => alg.TyUnit() } |||
        "String" ^^ { _ => alg.TyString() } |||
        "Float" ^^ { _ => alg.TyFloat() } |||
        "<" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ t0 => (x, t0) }, ",") <~ ">" ^^ alg.TyVariant |||
        "{" ~> repsep(lcid ~ (":" ~> t) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ alg.TyRecord
    }

    val pFullSimpleExtT: Alg[E, T] => (=> F) => PackratParser[T] = pFullSimpleExtT2 | pTypeVarT
  }

}

object FullSimple {

  trait Alg[E, T] extends TyArith.Alg[E, T] with Typed.Alg[E, T]
    with FullUntypedExt.Alg[E] with FullSimpleExt.Alg[E, T]

  trait Print extends Alg[String, String]
    with TyArith.Print with Typed.Print with FullUntypedExt.Print with FullSimpleExt.Print

  trait Parser[E, T, L <: {val pE : Util.PackratParser[E]; val pT : Util.PackratParser[T]}]
    extends TyArith.Parser[E, T, L] with Typed.Parser[E, T, L]
      with FullUntypedExt.Parser[E, L] with FullSimpleExt.Parser[E, T, L] {
    val pFullSimpleE = pTyArithE | pTypedE | pFullUntypedExtE | pFullSimpleExtE
    val pFullSimpleT = pTyArithT | pTypedT | pFullSimpleExtT
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
      "iszero (pred (succ (succ 0)))"
    ).foreach(parseAndPrint)
  }
}