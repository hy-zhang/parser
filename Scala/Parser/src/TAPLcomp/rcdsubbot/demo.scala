package TAPLcomp.rcdsubbot

import scala.io.Source

object RcdSubBotDemo {

  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/rcdsubbot.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = RcdSubBotParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}