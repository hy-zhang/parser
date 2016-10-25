package TAPLcomp.fullisorec

import scala.io.Source

object FullIsorecDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullisorec.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullIsorecParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}