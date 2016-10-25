package TAPLcomp.untyped

import scala.io.Source

object UntypedDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/untyped.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = UntypedParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}