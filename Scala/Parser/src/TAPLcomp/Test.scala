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
    val t0 = System.currentTimeMillis()
    lines.foreach(f)
    val t1 = System.currentTimeMillis()
    println("\n" + name + ", time: " + (t1 - t0) + "ms\n")
  }

  def parseAndPrint[E](parse: String => E, print: E => Document)(inp: String): Unit = {
    val width = 60
    val e = parse(inp)
    println(printDoc(print(e), width))
  }

  def main(args: Array[String]): Unit = {
    runTest("arith", parseAndPrint(ArithParsers.input, ArithPrinter.ptm))
    runTest("untyped", parseAndPrint(UntypedParsers.input, UntypedPrinter.ptm))
    runTest("fulluntyped", parseAndPrint(FullUntypedParsers.input, FullUntypedPrinter.ptm))
    runTest("tyarith", parseAndPrint(TyArithParsers.input, TyArithPrinter.ptm))
    runTest("simplebool", parseAndPrint(SimpleBoolParsers.input, SimpleBoolPrinter.ptm))
    runTest("fullsimple", parseAndPrint(FullSimpleParsers.input, FullSimplePrinter.ptm))
    runTest("bot", parseAndPrint(BotParsers.input, BotPrinter.ptm))
    runTest("fullref", parseAndPrint(FullRefParsers.input, FullRefPrinter.ptm))
    runTest("fullerror", parseAndPrint(FullErrorParsers.input, FullErrorPrinter.ptm))
    runTest("rcdsubbot", parseAndPrint(RcdSubBotParsers.input, RcdSubBotPrinter.ptm))
    runTest("fullsub", parseAndPrint(FullSubParsers.input, FullSubPrinter.ptm))
    runTest("fullequirec", parseAndPrint(FullEquiRecParsers.input, FullEquiRecPrinter.ptm))
    runTest("fullisorec", parseAndPrint(FullIsoRecParsers.input, FullIsoRecPrinter.ptm))
    runTest("equirec", parseAndPrint(EquiRecParsers.input, EquiRecPrinter.ptm))
    runTest("recon", parseAndPrint(ReconParsers.input, ReconPrinter.ptm))
    runTest("fullrecon", parseAndPrint(FullReconParsers.input, FullReconPrinter.ptm))
    runTest("fullpoly", parseAndPrint(FullPolyParsers.input, FullPolyPrinter.ptm))
    runTest("fullomega", parseAndPrint(FullOmegaParsers.input, FullOmegaPrinter.ptm))
  }
}
