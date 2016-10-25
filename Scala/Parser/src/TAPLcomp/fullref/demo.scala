package TAPLcomp.fullref

import scala.io.Source

object FullRefDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullref.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullRefParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}