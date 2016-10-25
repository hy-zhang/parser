package TAPLcomp.fullsub

import scala.io.Source

object FullSubDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/fullsub.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = FullSubParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}