package TAPLcomp.fullerror

import scala.io.Source

object FullErrorDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullerror.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullErrorParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}