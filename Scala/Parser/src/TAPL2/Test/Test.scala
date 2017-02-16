package TAPL2.Test

import TAPL2.Arith.TestArith
import TAPL2.Bot.TestBot
import TAPL2.FullEquiRec.TestFullEquiRec
import TAPL2.FullSimple.TestFullSimple
import TAPL2.FullUntyped.TestFullUntyped
import TAPL2.Untyped.TestUntyped
import TAPL2.FullRef.TestFullRef
import TAPL2.TyArith.TestTyArith
import TAPL2.SimpleBool.TestSimpleBool
import TAPL2.FullError.TestFullError
import TAPL2.FullSub.TestFullSub
import TAPL2.RcdSubBot.TestRcdSubBot
import TAPL2.FullIsoRec.TestFullIsoRec
import TAPL2.EquiRec.TestEquiRec
import TAPL2.FullOmega.TestFullOmega
import TAPL2.FullPoly.TestFullPoly
import TAPL2.Recon.TestRecon
import TAPL2.FullRecon.TestFullRecon

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
