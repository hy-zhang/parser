package TAPLcomp.equirec

import scala.io.Source

object EquirecDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/equirec.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = EquirecParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}