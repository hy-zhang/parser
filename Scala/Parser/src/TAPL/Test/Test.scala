package TAPL.Test

import TAPL.{TestArith, TestUntyped}

import scala.io.Source

object Test {

  def runTest(name: String, f: => (String => Unit)): Unit = {
    time {
      val inputFile = "examples/" + name + ".tapl"
      val lines: List[String] = Source.fromFile(inputFile).getLines().toList
      lines.foreach(f)
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def main(args: Array[String]): Unit = {
    //runTest("arith", TestArith.parseAndPrint)
    runTest("untyped", TestUntyped.parseAndPrint)
    //runTest("fulluntyped", )
  }

}
