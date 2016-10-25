package TAPLcomp.fullsimple

import scala.io.Source

object FullSimpleDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullsimple.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullSimpleParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}