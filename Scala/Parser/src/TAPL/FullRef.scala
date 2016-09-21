package TAPL

/**
  * Created by Huang on 9/21/16.
  */
object FullRef {

  import Util._

  trait Lexer {
    lexical.reserved += ("Top", "Bot", "ref", "Ref", "Source", "Sink")
    lexical.delimiters += ("!", ":=")
  }

  type ParserSig[E, T] = {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
  }

  trait FullRefAlg[E, T] {
    def TyTop(): T

    def TyBot(): T

    def TyRef(t: T): T

    def TmRef(e: E): E

    def TmDeRef(e: E): E

    def TmAssign(l: E, r: E): E

    def TySource(t: T): T

    def TySink(t: T): T
  }

  trait PrettyPrint extends FullRefAlg[String, String] {
    def TyTop() = "Top"

    def TyBot() = "Bot"

    def TyRef(t: String) = "Ref " + t

    def TmRef(e: String) = "ref " + e

    def TmDeRef(e: String) = "!" + e

    def TmAssign(l: String, r: String) = l + " := " + r

    def TySource(t: String) = "Source " + t

    def TySink(t: String) = "Sink " + t
  }

  trait Parser[E, T, F <: ParserSig[E, T]] {
    lazy val pE: FullRefAlg[E, T] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val t = l.pT

      List(
        "ref" ~> e ^^ { e => alg.TmRef(e) },
        "!" ~> e ^^ { e => alg.TmDeRef(e) },
        e ~ (":=" ~> e) ^^ { case lhs ~ rhs => alg.TmAssign(lhs, rhs) }
      ).reduce((a, b) => a ||| b)
    }

    lazy val pT: FullRefAlg[E, T] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT

      List(
        "Top" ^^ { _ => alg.TyTop() },
        "Bot" ^^ { _ => alg.TyBot() },
        "Ref" ~> t ^^ (t => alg.TyRef(t)),
        "Source" ~> t ^^ { t => alg.TySource(t) },
        "Sink" ~> t ^^ { t => alg.TySink(t) },
        "(" ~> t <~ ")"
      ).reduce((a, b) => a ||| b)
    }
  }

  def pET[F <: ParserSig[String, String]] = {
    (new Parser[String, String, F]() {}.pE(new PrettyPrint() {}), new Parser[String, String, F]() {}.pT(new PrettyPrint() {}))
  }
}

object TestFullRef extends Arith.Lexer with FullUntyped.Lexer with TyArith.Lexer with SimpleBool.Lexer with FullSimple.Lexer with FullRef.Lexer {

  import Util._

  trait ParserSig[E, T] {
    val pE: PackratParser[E]
    val pT: PackratParser[T]
  }

  trait Parse[E, T] {
    type L = ParserSig[E, T]
    type PE = (=> L) => PackratParser[E]
    type PT = (=> L) => PackratParser[T]

    val pArithE: PE
    val pFullUntypedE: PE
    val pTyArithT: PT
    val pSimpleBoolET: (PE, PT)
    val pFullSimpleET: (PE, PT)
    val pFullRefET: (PE, PT)

    val p: (=> L) => L = l => new L() {
      override lazy val pE = List(pArithE, pFullUntypedE, pSimpleBoolET._1, pFullSimpleET._1, pFullRefET._1).reduce(alt[E, L])(l)
      override lazy val pT = List(pTyArithT, pSimpleBoolET._2, pFullSimpleET._2, pFullRefET._2).reduce(alt[T, L])(l)
    }
  }

  lazy val parser = new Parse[String, String]() {
    type R = ParserSig[String, String]

    lazy val pArithE = Arith.pE[R]
    lazy val pFullUntypedE = FullUntyped.pE[R]
    lazy val pTyArithT = TyArith.pT[R]
    lazy val pSimpleBoolET = SimpleBool.pET[R]
    lazy val pFullSimpleET = FullSimple.pET[R]
    lazy val pFullRefET = FullRef.pET[R]
  }

  lazy val parse = runParser(fix(parser.p).pE)

  def main(args: Array[String]) = {
    parse("\\a:Ref (Nat->Nat).\\n:Nat.(!a n)")
    parse("\\a:Unit.ref (\\n:Nat.0)")
    parse("\\a:Ref (Nat->Nat).\\m:Nat.\\n:Nat.let oldf = !a in a := (\\n:Nat.if true then v else (oldf n))")
    parse("\\x:Sink Bool.unit")
    parse("(\\x:Bool->Bool.if x false then true else false) (\\x:Bool.if x then false else true)")
  }
}
