package TAPLcomp2

import TAPLcomp2.Print._
import TAPLcomp2.arith.{ArithParsers, ArithPrinter}
import TAPLcomp2.bot.{BotParsers, BotPrinter}
import TAPLcomp2.equirec.{EquiRecParsers, EquiRecPrinter}
import TAPLcomp2.fullequirec.{FullEquiRecParsers, FullEquiRecPrinter}
import TAPLcomp2.fullerror.{FullErrorParsers, FullErrorPrinter}
import TAPLcomp2.fullisorec.{FullIsoRecParsers, FullIsoRecPrinter}
import TAPLcomp2.fullomega.{FullOmegaParsers, FullOmegaPrinter}
import TAPLcomp2.fullpoly.{FullPolyParsers, FullPolyPrinter}
import TAPLcomp2.fullrecon.{FullReconParsers, FullReconPrinter}
import TAPLcomp2.fullref.{FullRefParsers, FullRefPrinter}
import TAPLcomp2.fullsimple.{FullSimpleParsers, FullSimplePrinter}
import TAPLcomp2.fullsub.{FullSubParsers, FullSubPrinter}
import TAPLcomp2.fulluntyped.{FullUntypedParsers, FullUntypedPrinter}
import TAPLcomp2.rcdsubbot.{RcdSubBotParsers, RcdSubBotPrinter}
import TAPLcomp2.recon.{ReconParsers, ReconPrinter}
import TAPLcomp2.simplebool.{SimpleBoolParsers, SimpleBoolPrinter}
import TAPLcomp2.tyarith.{TyArithParsers, TyArithPrinter}
import TAPLcomp2.untyped.{UntypedParsers, UntypedPrinter}

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
