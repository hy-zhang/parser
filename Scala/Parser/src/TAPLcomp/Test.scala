package TAPLcomp

import TAPLcomp.Print._
import TAPLcomp.arith.{ArithParsers, ArithPrinter}
import TAPLcomp.bot.{BotParsers, BotPrinter}
import TAPLcomp.equirec.{EquiRecParsers, EquiRecPrinter}
import TAPLcomp.fullequirec.{FullEquiRecParsers, FullEquiRecPrinter}
import TAPLcomp.fullerror.{FullErrorParsers, FullErrorPrinter}
import TAPLcomp.fullisorec.{FullIsoRecParsers, FullIsoRecPrinter}
import TAPLcomp.fullomega.{FullOmegaParsers, FullOmegaPrinter}
import TAPLcomp.fullpoly.{FullPolyParsers, FullPolyPrinter}
import TAPLcomp.fullrecon.{FullReconParsers, FullReconPrinter}
import TAPLcomp.fullref.{FullRefParsers, FullRefPrinter}
import TAPLcomp.fullsimple.{FullSimpleParsers, FullSimplePrinter}
import TAPLcomp.fullsub.{FullSubParsers, FullSubPrinter}
import TAPLcomp.fulluntyped.{FullUntypedParsers, FullUntypedPrinter}
import TAPLcomp.rcdsubbot.{RcdSubBotParsers, RcdSubBotPrinter}
import TAPLcomp.recon.{ReconParsers, ReconPrinter}
import TAPLcomp.simplebool.{SimpleBoolParsers, SimpleBoolPrinter}
import TAPLcomp.tyarith.{TyArithParsers, TyArithPrinter}
import TAPLcomp.untyped.{UntypedParsers, UntypedPrinter}

import scala.io.Source
import scala.text.Document

object Test {

  def runTest(name: String, f: => (String => Unit)): Unit = {
    val inputFile = "examples/" + name + ".tapl"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    val t0 = System.nanoTime()
    lines.foreach(f)
    val t1 = System.nanoTime()
    println((t1 - t0) / 1000000)
  }

  def parseAndPrint[E](parse: String => E, print: E => Document)(inp: String): Unit = {
    val width = 60
    val e = parse(inp)
    println(printDoc(print(e), width))
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      sys.error("No argument")
    } else {
      val name = args(0).toLowerCase
      val fn: String => Unit = name match {
        case "arith" => parseAndPrint(ArithParsers.input, ArithPrinter.ptm)
        case "untyped" => parseAndPrint(UntypedParsers.input, UntypedPrinter.ptm)
        case "fulluntyped" => parseAndPrint(FullUntypedParsers.input, FullUntypedPrinter.ptm)
        case "tyarith" => parseAndPrint(TyArithParsers.input, TyArithPrinter.ptm)
        case "simplebool" => parseAndPrint(SimpleBoolParsers.input, SimpleBoolPrinter.ptm)
        case "fullsimple" => parseAndPrint(FullSimpleParsers.input, FullSimplePrinter.ptm)
        case "bot" => parseAndPrint(BotParsers.input, BotPrinter.ptm)
        case "fullref" => parseAndPrint(FullRefParsers.input, FullRefPrinter.ptm)
        case "fullerror" => parseAndPrint(FullErrorParsers.input, FullErrorPrinter.ptm)
        case "rcdsubbot" => parseAndPrint(RcdSubBotParsers.input, RcdSubBotPrinter.ptm)
        case "fullsub" => parseAndPrint(FullSubParsers.input, FullSubPrinter.ptm)
        case "fullequirec" => parseAndPrint(FullEquiRecParsers.input, FullEquiRecPrinter.ptm)
        case "fullisorec" => parseAndPrint(FullIsoRecParsers.input, FullIsoRecPrinter.ptm)
        case "equirec" => parseAndPrint(EquiRecParsers.input, EquiRecPrinter.ptm)
        case "recon" => parseAndPrint(ReconParsers.input, ReconPrinter.ptm)
        case "fullrecon" => parseAndPrint(FullReconParsers.input, FullReconPrinter.ptm)
        case "fullpoly" => parseAndPrint(FullPolyParsers.input, FullPolyPrinter.ptm)
        case "fullomega" => parseAndPrint(FullOmegaParsers.input, FullOmegaPrinter.ptm)
        case _ => sys.error("Incorrect name")
      }
      runTest(name, fn)
    }
  }
}
