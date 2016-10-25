package TAPLcomp.fullomega

import scala.io.Source

object FullOmegaDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullomega.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullOmegaParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}