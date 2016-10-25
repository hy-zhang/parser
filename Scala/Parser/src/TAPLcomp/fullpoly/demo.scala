package TAPLcomp.fullpoly

import scala.io.Source

object FullPolyDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullpoly.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullPolyParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}