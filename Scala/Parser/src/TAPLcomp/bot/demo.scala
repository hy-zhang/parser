package TAPLcomp.bot

import scala.io.Source

object BotDemo {
  import TAPLcomp.Print._

  val width = 60

  def main(args: Array[String]): Unit = {
    val inFile = "examples/bot.tapl"
    for (line <- Source.fromFile(inFile).getLines()) {
      val e = BotParsers.input(line)
      println(print(PrettyPrinter.ptm(e), width))
    }
  }

}