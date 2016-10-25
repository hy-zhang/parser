package TAPLcomp.fullrecon

import scala.io.Source

object FullReconDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullrecon.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullReconParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}