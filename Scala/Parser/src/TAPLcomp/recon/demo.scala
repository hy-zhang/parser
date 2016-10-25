package TAPLcomp.recon

import scala.io.Source

object ReconDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/recon.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = ReconParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}