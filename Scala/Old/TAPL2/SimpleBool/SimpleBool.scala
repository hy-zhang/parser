package TAPL2.SimpleBool

import TAPL2.TyArith.TypedBool
import TAPL2.Untyped.VarApp
import TAPL2.Util._
import TAPL2.{Term, Ty}

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyBool extends Ty

/* <5> */
object Typed {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] extends VarApp.Parser[F] {
    lexical.delimiters += ("\\", ".", "(", ")", ":", "->")

    private val pAbsE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE
      lazy val t = l.pT

      ("\\" ~> lcid) ~ (":" ~> t) ~ ("." ~> e) ^^ { case x ~ t0 ~ e0 => TmAbs(x, t0, e0) } |||
        "(" ~> e <~ ")"
    }

    val pTypedE: (=> F) => PackratParser[Term] = l => pVarAppE(l) ||| pAbsE(l)

    val pTypedT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      t ~ ("->" ~> t) ^^ { case t1 ~ t2 => TyArr(t1, t2) } ||| "(" ~> t <~ ")"
    }
  }

}

object SimpleBool {

  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends Typed.Parser[L] with TypedBool.Parser[L] {
    val pSimpleBoolE: (=> L) => PackratParser[Term] = l => pTypedE(l) ||| pTypedBoolE(l)
    val pSimpleBoolT: (=> L) => PackratParser[Ty] = l => pTypedT(l) ||| pTypedBoolT(l)
  }

}

object TestSimpleBool {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new SimpleBool.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pSimpleBoolE(l), lang.pSimpleBoolT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}