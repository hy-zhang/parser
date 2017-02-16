package TAPL

import TAPL.Lib._


object TypedNat {

  trait Alg[E, T] extends Nat.Alg[E] {
    def TyNat(): T
  }

  trait Print extends Alg[String, String] with Nat.Print {
    def TyNat() = "Nat"
  }

  trait Parse[E, T] extends Nat.Parse[E] {
    lexical.reserved += "Nat"

    override val alg: Alg[E, T]

    val pTypedNatE: Parser[E] = pNatE
    val pTypedNatT: Parser[T] = "Nat" ^^ { _ => alg.TyNat() }
  }

}

object TypedBool {

  trait Alg[E, T] extends Bool.Alg[E] {
    def TyBool(): T
  }

  trait Print extends Alg[String, String] with Bool.Print {
    def TyBool() = "Bool"
  }

  trait Parse[E, T] extends Bool.Parse[E] {
    lexical.reserved += "Bool"

    override val alg: Alg[E, T]

    val pTypedBoolE: Parser[E] = pBoolE
    val pTypedBoolT: Parser[T] = "Bool" ^^ { _ => alg.TyBool() }
  }

}

trait TParser[T] {
  val pT: Parser[T]
}

trait ETParser[E, T] extends EParser[E] with TParser[T]


object TyArith {

  trait Alg[E, T] extends TypedBool.Alg[E, T] with TypedNat.Alg[E, T]

  trait Print extends Alg[String, String] with TypedBool.Print with TypedNat.Print

  trait Parse[E, T] extends ETParser[E, T] with TypedBool.Parse[E, T] with TypedNat.Parse[E, T] {
    override val alg: Alg[E, T]

    val pTyArithE: Parser[E] = pTypedBoolE ||| pTypedNatE
    val pTyArithT: Parser[T] = pTypedBoolT ||| pTypedNatT

    override val pE: Parser[E] = pTyArithE

    override val pT: Parser[T] = pTyArithT
  }

}

object TestTyArith {

  def parseWithAlg[E, T](inp: String)(a: TyArith.Alg[E, T]): E = {
    val p = new TyArith.Parse[E, T] {
      override val alg: TyArith.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new TyArith.Print {}))

}