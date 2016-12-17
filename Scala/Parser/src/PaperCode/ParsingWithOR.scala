package PaperCode

import TAPL.Util._

object ParsingWithOR {

//BEGIN_PARSING_PVAR
type Parser[E] = PackratParser[E]

abstract class Expr
case class Var(x : String) extends Expr
val pVar : Parser[Expr] = ident ^^ Var
//END_PARSING_PVAR

//BEGIN_PARSING_PAPP
case class App(e1 : Expr, e2 : Expr) extends Expr
val pApp : Parser[Expr] = pVar ~ pVar ^^ { case e1 ~ e2 => App(e1, e2) }
//END_PARSING_PAPP

}

object ParsingWithOR2 {
  
class Expr
case class App(e1 : Expr, e2 : Expr) extends Expr
val pVar : Parser[Expr] = ???
  
//BEGIN_PARSING_PAPP2
val pApp : Parser[Expr] = {
  val p = pVar ||| pApp
  p ~ p ^^ { case e1 ~ e2 => App(e1, e2) }
}
//END_PARSING_PAPP2

}

object ParsingWithOR3 {

class Expr
case class App(e1 : Expr, e2 : Expr) extends Expr
val pVar : Parser[Expr] = ???

//BEGIN_PARSING_PLIT
case class Lit(n : Int) extends Expr
val pLit : Parser[Expr] = numericLit ^^ { x => Lit(x.toInt) }
val pApp : Parser[Expr] = {
  val p = pVar ||| pApp ||| pLit
  p ~ p ^^ { case e1 ~ e2 => App(e1, e2) }
}
//END_PARSING_PLIT
  
}

object ParsingWithOR4 {
  
type Fix[T] = (=> T) => T
class Expr
case class App(e1 : Expr, e2 : Expr) extends Expr
case class Var(x : String) extends Expr
def merge[E, F, G, H[_,_]](op : F => G => H[F, G], x : (=> E) => F, y : (=> E) => G) : (=> E) => H[F, G] = e => op(x(e))(y(e))

//BEGIN_PARSING_PAPPFIX
val pApp : Fix[Parser[Expr]] = p => p ~ p ^^ { case e1 ~ e2 => App(e1, e2) }
//END_PARSING_PAPPFIX

//BEGIN_PARSING_COMBINATOR
val pVar : Fix[Parser[Expr]] = p => ident ^^ Var

implicit class Combinator[E, R](x : (=> R) => Parser[E]) {
  type H[A, B] = Parser[E]
  def |||(y : (=> R) => Parser[E]) : (=> R) => Parser[E] = {
    val alt : Parser[E] => Parser[E] => Parser[E] = a => b => a ||| b
    merge[R, Parser[E], Parser[E], H](alt, x, y)
  }
}
//END_PARSING_COMBINATOR

//BEGIN_PARSING_PVARAPP
val pVarApp : Fix[Parser[Expr]] = pVar ||| pApp
//END_PARSING_PVARAPP

//BEGIN_PARSING_TYPEDEXPR
type Pair = (Parser[Expr], Parser[Type])

abstract class Type
case class IntT() extends Type
val pInt : (=> Pair) => Parser[Type] =
  p => "Int" ^^^ IntT()

case class Lam(x : String, t : Type, e : Expr) extends Expr
val pLam : (=> Pair) => Parser[Expr] =
  p => ("\\" ~> ident) ~ (":" ~> p._2) ~ ("." ~> p._1) ^^
        { case x ~ t ~ e => Lam(x, t, e) }

val pVar2 : (=> Pair) => Parser[Expr] = p => pVar(p._1)
val pApp2 : (=> Pair) => Parser[Expr] = p => pApp(p._1)

val pET : Fix[Pair] = {
  val pE = pVar2 ||| pApp2 ||| pLam
  val pT = pInt
  p => (pE(p), pT(p))
}
//END_PARSING_TYPEDEXPR

}