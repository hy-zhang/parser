package TAPLcomp.fullequirec

import scala.io.Source

object FullEquirecDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullequirec.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullEquirecParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}