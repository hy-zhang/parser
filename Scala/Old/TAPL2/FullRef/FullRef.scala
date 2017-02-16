package TAPL2.FullRef

import TAPL2.Util._
import TAPL2.{Term, Ty}
import TAPL2.FullSimple.FullSimple
import TAPL2.Bot.TopBot

case class TyVar(i: String) extends Ty

case class TyArr(t1: Ty, t2: Ty) extends Ty

case object TyUnit extends Ty

case class TyRecord(els: List[(String, Ty)]) extends Ty

case class TyVariant(els: List[(String, Ty)]) extends Ty

case object TyBool extends Ty

case object TyString extends Ty

case object TyFloat extends Ty

case object TyNat extends Ty

case class TyRef(ty: Ty) extends Ty

case object TyTop extends Ty

case object TyBot extends Ty

case class TySource(ty: Ty) extends Ty

case class TySink(ty: Ty) extends Ty

case object TmTrue extends Term

case object TmFalse extends Term

case class TmIf(cond: Term, t1: Term, t2: Term) extends Term

case class TmCase(sel: Term, branches: List[(String, String, Term)]) extends Term

case class TmTag(tag: String, t: Term, ty: Ty) extends Term

case class TmVar(i: String) extends Term

case class TmAbs(v: String, ty: Ty, t: Term) extends Term

case class TmApp(t1: Term, t2: Term) extends Term

case class TmLet(l: String, t1: Term, t2: Term) extends Term

case class TmFix(t: Term) extends Term

case class TmString(s: String) extends Term

case object TmUnit extends Term

case class TmAscribe(t: Term, ty: Ty) extends Term

case class TmRecord(fields: List[(String, Term)]) extends Term

case class TmProj(t: Term, proj: String) extends Term

case object TmZero extends Term

case class TmSucc(t: Term) extends Term

case class TmPred(t: Term) extends Term

case class TmIsZero(t: Term) extends Term

case class TmInert(ty: Ty) extends Term

case class TmLoc(i: Int) extends Term

case class TmRef(t: Term) extends Term

case class TmDeRef(t: Term) extends Term

case class TmAssign(t1: Term, t2: Term) extends Term

/* <7> */
object Ref {

  trait Parser[F <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}] {
    lexical.reserved += ("ref", "Ref")
    lexical.delimiters += ("!", ":=")

    val pRefE: (=> F) => PackratParser[Term] = l => {
      lazy val e = l.pE

      "ref" ~> e ^^ { e => TmRef(e) } |||
        "!" ~> e ^^ { e => TmDeRef(e) } |||
        e ~ (":=" ~> e) ^^ { case lhs ~ rhs => TmAssign(lhs, rhs) }
    }

    val pRefT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      "Ref" ~> t ^^ TyRef
    }
  }

}

object SourceSink {

  trait Parser[F <: {val pT : PackratParser[Ty]}] {
    lexical.reserved += ("Source", "Sink")

    val pSourceSinkT: (=> F) => PackratParser[Ty] = l => {
      lazy val t = l.pT

      "Source" ~> t ^^ TySource ||| "Sink" ~> t ^^ TySink
    }
  }

}

object FullRef {
  
  trait Parser[L <: {val pE : PackratParser[Term]; val pT : PackratParser[Ty]}]
    extends FullSimple.Parser[L] with TopBot.Parser[L] with Ref.Parser[L] with SourceSink.Parser[L] {
    val pFullRefE: (=> L) => PackratParser[Term] =
      l => pFullSimpleE(l) ||| pRefE(l)
    val pFullRefT: (=> L) => PackratParser[Ty] =
      l => pFullSimpleT(l) ||| pRefT(l) ||| pTopBotT(l) ||| pSourceSinkT(l)
  }

}

object TestFullRef {

  class List[E, T](pe: PackratParser[E], pt: PackratParser[T]) {
    val pE = pe
    val pT = pt
  }

  def parseAndPrint(inp: String) = {
    def parser(l: => List[Term, Ty]): List[Term, Ty] = {
      val lang = new FullRef.Parser[List[Term, Ty]] {}
      new List[Term, Ty](lang.pFullRefE(l), lang.pFullRefT(l))
    }

    val t = phrase(fix(parser).pE)(new lexical.Scanner(inp))
    if (t.successful) println(t.get) else scala.sys.error(t.toString)
  }

}