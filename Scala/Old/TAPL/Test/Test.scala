package TAPL.Test

import TAPL._

import scala.io.Source

object Test {

  def runTest(name: String, f: => (String => Unit)): Unit = {
    val inputFile = "examples/" + name + ".tapl"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    val t0 = System.nanoTime()
    lines.foreach(f)
    val t1 = System.nanoTime()
    println((t1 - t0) / 1000000)
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      sys.error("No argument")
    } else {
      val name = args(0).toLowerCase
      val fn: String => Unit = name match {
        case "arith" => TestArith.parseAndPrint
        case "untyped" => TestUntyped.parseAndPrint
        case "fulluntyped" => TestFullUntyped.parseAndPrint
        case "tyarith" => TestTyArith.parseAndPrint
        case "simplebool" => TestSimpleBool.parseAndPrint
        case "fullsimple" => TestFullSimple.parseAndPrint
        case "bot" => TestBot.parseAndPrint
        case "fullref" => TestFullRef.parseAndPrint
        case "fullerror" => TestFullError.parseAndPrint
        case "rcdsubbot" => TestRcdSubBot.parseAndPrint
        case "fullsub" => TestFullSub.parseAndPrint
        case "fullequirec" => TestFullEquiRec.parseAndPrint
        case "fullisorec" => TestFullIsoRec.parseAndPrint
        case "equirec" => TestEquiRec.parseAndPrint
        case "recon" => TestRecon.parseAndPrint
        case "fullrecon" => TestFullRecon.parseAndPrint
        case "fullpoly" => TestFullPoly.parseAndPrint
        case "fullomega" => TestFullOmega.parseAndPrint
        case _ => sys.error("Incorrect name")
      }
      runTest(name, fn)
    }
  }

}
