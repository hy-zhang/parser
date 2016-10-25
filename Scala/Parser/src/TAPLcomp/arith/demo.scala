package TAPLcomp.arith

import scala.io.Source

object ArithDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/arith.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = ArithParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }
}