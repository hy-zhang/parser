package TAPL

object Util {
  def fix[A](f: (=> A) => A): A = { lazy val a: A = f(a); a }
}