package TAPL

/* <18> */
object FullOmega {
  import Util._
  trait FullOmegaAlg[E, T, K] {
    def KnStar(): K
    def KnArr(k1: K, k2: K): K
    def TyAll2(x: String, k: K, t: T): T
    def TySome2(x: String, k: K, t: T): T
    def TmTAbs2(x: String, k: K, e: E): E
    // Missed some cases?
  }
  trait Lexer extends FullOmegaAlg[String, String, String] {
    lexical.reserved += ("Star", "All", "Some")
    lexical.delimiters += ("=>", ":", ".", ",", "{", "}")
    override def KnStar(): String = "Star"
    override def KnArr(k1: String, k2: String): String = k1 + "=>" + k2
    override def TyAll2(x: String, k: String, t: String): String = "All " + x + ":" + k + "." + t
    override def TySome2(x: String, k: String, t: String): String = "{Some " + x + ":" + k + "," + t + "}"
    override def TmTAbs2(x: String, k: String, e: String): String = "\\(" + x + ":" + k + ")." + e
  }
  trait Parser[E, T, K, F <: {val pE: PackratParser[E]; val pT: PackratParser[T]; val pK: PackratParser[K]}] {
    lazy val pE: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[E] = alg => l => {
      lazy val e = l.pE
      lazy val k = l.pK
      "\\" ~> ucid ~ (":" ~> k) ~ ("." ~> e) ^^ { case x ~ kn ~ ex => alg.TmTAbs2(x, kn, ex) }
    }
    lazy val pT: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[T] = alg => l => {
      lazy val t = l.pT
      lazy val k = l.pK
      List(
        "All" ~> ucid ~ (":" ~> k) ~ ("." ~> t) ^^ { case x ~ kn ~ ty => alg.TyAll2(x, kn, ty) },
        ("{" ~> "Some" ~> ucid ~ (":" ~> k) ~ ("," ~> t) <~ "}") ^^ { case x ~ kn ~ ty => alg.TySome2(x, kn, ty) }
      ).reduce((a, b) => a ||| b)
    }
    lazy val pK: FullOmegaAlg[E, T, K] => (=> F) => PackratParser[K] = alg => l => {
      lazy val k = l.pK
      List(
        "Star" ^^ { _ => alg.KnStar() },
        k ~ ("=>" ~> k) ^^ { case k1 ~ k2 => alg.KnArr(k1, k2) }
      ).reduce((a, b) => a ||| b)
    }
  }
}

trait FullOmegaLNG[E, T, K, L <: {val pE: Util.PackratParser[E]; val pT: Util.PackratParser[T]; val pK: Util.PackratParser[K]}] extends FullPolyLNG[E, T, L] with FullOmega.Lexer {
  val pFullOmegaETK = new FullOmega.Parser[E, T, K, L](){}
  val pFullOmegaLNGE = pFullPolyLNGE | pFullOmegaETK.pE
  val pFullOmegaLNGT = pFullPolyLNGT | pFullOmegaETK.pT
  val pFullOmegaLNGK = pFullOmegaETK.pK
}

object TestFullOmegaLNG {
  import Util._
  class List[E, T, K](pe : PackratParser[E], pt : PackratParser[T], pk : PackratParser[K]) { val pE = pe; val pT = pt; val pK = pk }
  object Test extends FullOmegaLNG[String, String, String, List[String, String, String]] {
    lazy val parser : (=> List[String, String, String]) => List[String, String, String] = l =>
      new List[String, String, String](pFullOmegaLNGE(Test)(l), pFullOmegaLNGT(Test)(l), pFullOmegaLNGK(Test)(l))
    lazy val parse = runParser(Util.fix(parser).pE)
  }
  def main(args : Array[String]) = {
    List(
      "fold [Counter] {get=unit, inc=unit}",
      "(unfold [Counter] p).get",
      "x",
      "if x then false else x",
      "\\x:A.x",
      "\\X:Star=>Star.X",
      "(\\X:Star.\\x:X.x) [All X:Star.X->X]",
      "\\x:({Some X:Star, {c:X, f:X->Nat}}).x",
      "{*All Y:Star.Y, \\x:All Y:Star.Y.x} as {Some X:Star, X->X}",
      "{*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}}",
      "let {X,ops} = {*Nat, {c=0, f=\\x:Nat.succ (x)}} as {Some X:Star, {c:X, f:X->Nat}} in (ops.f ops.c)"
    ).foreach(Test.parse)
  }
}