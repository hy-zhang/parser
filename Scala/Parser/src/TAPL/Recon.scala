package TAPL

import TAPL.Lib._


object Recon {

  trait Alg[E, T] extends Typed.Alg[E, T] with TyArith.Alg[E, T] with TypeVar.Alg[T]

  trait Print extends Alg[String, String] with Typed.Print with TyArith.Print with TypeVar.Print

  trait Parse[E, T] extends TyArith.Parse[E, T] with Typed.Parse[E, T] with TypeVar.Parse[T] {
    override val alg: Alg[E, T]

    val pReconE: Parser[E] = pTypedE ||| pTyArithE
    val pReconT: Parser[T] = pTypedT ||| pTyArithT ||| pTypeVarT

    override val pE: Parser[E] = pReconE
    override val pT: Parser[T] = pReconT
  }

}

object TestRecon {

  def parseWithAlg[E, T](inp: String)(a: Recon.Alg[E, T]): E = {
    val p = new Recon.Parse[E, T] {
      override val alg: Recon.Alg[E, T] = a
    }
    parse(p.pE)(inp)
  }

  def parseAndPrint(inp: String): Unit = println(parseWithAlg(inp)(new Recon.Print {}))

}