package TAPLcomp.fulluntyped

import scala.io.Source

object FullUntypedDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fulluntyped.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullUntypedParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }
}