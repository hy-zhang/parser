package TAPLcomp.tyarith

import scala.io.Source

object TyArithDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/tyarith.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = TyArithParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }
}