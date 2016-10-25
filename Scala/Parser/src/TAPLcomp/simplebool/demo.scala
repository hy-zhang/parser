package TAPLcomp.simplebool

import scala.io.Source

object SimpleBoolDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/simplebool.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = SimpleBoolParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }
}