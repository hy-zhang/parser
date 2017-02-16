package TAPL2.FullRef

import TAPL2.Lib._
import TAPL2.Bot.TopBot
import TAPL2.FullSimple.FullSimple


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


object Ref {

  trait Parser extends ETParser {
    lexical.reserved += ("ref", "Ref")
    lexical.delimiters += ("!", ":=")

    val pRefE: PackratParser[Term] = {
      "ref" ~> pE ^^ { e => TmRef(e) } |||
        "!" ~> pE ^^ { e => TmDeRef(e) } |||
        pE ~ (":=" ~> pE) ^^ { case lhs ~ rhs => TmAssign(lhs, rhs) }
    }

    val pRefT: PackratParser[Ty] = "Ref" ~> pT ^^ TyRef
  }

}

object SourceSink {

  trait Parser extends TParser {
    lexical.reserved += ("Source", "Sink")

    val pSourceSinkT: PackratParser[Ty] =
      "Source" ~> pT ^^ TySource |||
        "Sink" ~> pT ^^ TySink
  }

}

object FullRef {

  trait Parser extends FullSimple.Parser with TopBot.Parser
    with Ref.Parser with SourceSink.Parser {

    val pFullRefE: PackratParser[Term] = pFullSimpleE ||| pRefE
    val pFullRefT: PackratParser[Ty] = pFullSimpleT ||| pRefT ||| pTopBotT ||| pSourceSinkT

    override val pE: PackratParser[Term] = pFullRefE
    override val pT: PackratParser[Ty] = pFullRefT
  }

}

object TestFullRef {

  def parseAndPrint(inp: String): Unit = {
    val p = new FullRef.Parser {}
    println(parse(p.pE)(inp))
  }

}