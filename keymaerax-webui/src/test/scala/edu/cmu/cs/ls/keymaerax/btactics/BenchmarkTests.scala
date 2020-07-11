/**
  * Copyright (c) Carnegie Mellon University.
  * See LICENSE.txt for the conditions of this license.
  */
package edu.cmu.cs.ls.keymaerax.btactics

import java.io.PrintWriter

import edu.cmu.cs.ls.keymaerax.Configuration
import edu.cmu.cs.ls.keymaerax.bellerophon.TacticStatistics
import edu.cmu.cs.ls.keymaerax.bellerophon.parser.BelleParser
import edu.cmu.cs.ls.keymaerax.btactics.BenchmarkTests._
import edu.cmu.cs.ls.keymaerax.btactics.InvariantGenerator.GenProduct
import edu.cmu.cs.ls.keymaerax.core.{Box, False, Formula, Imply, ODESystem, Program, Sequent, SuccPos}
import edu.cmu.cs.ls.keymaerax.hydra.DatabasePopulator
import edu.cmu.cs.ls.keymaerax.parser.KeYmaeraXArchiveParser.{Declaration, ParsedArchiveEntry}
import edu.cmu.cs.ls.keymaerax.parser._
import edu.cmu.cs.ls.keymaerax.tags.ExtremeTest
import edu.cmu.cs.ls.keymaerax.tools.ToolOperationManagement

import scala.language.postfixOps
import org.scalatest.{AppendedClues, Suites}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.{Seconds, Span}

import scala.collection.immutable.{IndexedSeq, Map, Nil}
import scala.reflect.io.File

/**
  * Benchmarks.
  * Created by smitsch on 4/26/18.
  */
@ExtremeTest
class BenchmarkTests extends Suites(
  // benchmark problems from tactics and with database recording
//  new TutorialRegressionTester("Basic Benchmark", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/basic.kyx"),
//  new TutorialRegressionTester("Advanced Benchmark", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/advanced.kyx"),
//  new TutorialRegressionTester("Nonlinear Benchmark", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/nonlinear.kyx")
  // export
//  new BenchmarkExporter("Export", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/basic.kyx")
  // benchmark problems
//  new BenchmarkExporter("Basic", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/basic.kyx"),
//  new BenchmarkExporter("Advanced", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/advanced.kyx"),
//  new BenchmarkExporter("Nonlinear", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/nonlinear.kyx"),
  new BenchmarkTester("Basic", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/basic.kyx", 300, genCheck=false),
  new BenchmarkTester("Games", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/games.kyx", 300, genCheck=false),
//  new BenchmarkTester("Counterexamples", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/counterexample.kyx", 300, genCheck=false),
  new BenchmarkTester("Advanced", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/advanced.kyx", 1500, genCheck=false),
  new BenchmarkTester("Nonlinear", s"$GITHUB_PROJECTS_RAW_PATH/benchmarks/nonlinear.kyx", 300, genCheck=true)
)

object BenchmarkTests {
  private val GITHUB_PROJECTS_RAW_PATH = "https://raw.githubusercontent.com/LS-Lab/KeYmaeraX-projects/master"
  // for testing changes in a locally cloned repository
//  private val GITHUB_PROJECTS_RAW_PATH = "classpath:/keymaerax-projects"
}

/** Collects a benchmark result. */
case class BenchmarkResult(name: String, status: String, timeout: Int, totalDuration: Long, qeDuration: Long,
                           invGenDuration: Long, invCheckDuration: Long,
                           proofSteps: Int, tacticSize: Int,
                           ex: Option[Throwable], info: Any = Map.empty) {
  override def toString: String =
    s"""Proof Statistics ($name $status, with time budget $timeout)
      |Duration [ms]: $totalDuration
      |QE [ms]: $qeDuration
      |Inv Gen[ms]: $invGenDuration
      |Inv Check[ms]: $invCheckDuration
      |Proof steps: $proofSteps
      |Tactic size: $tacticSize
    """.stripMargin

  def toCsv(infoPrinter: Any=>String = (_: Any)=>""): String = s"$name,$status,$timeout,$totalDuration,$qeDuration,$invGenDuration,$invCheckDuration,$proofSteps,$tacticSize" + infoPrinter(info)
}

/** Exports to different formats */
class BenchmarkExporter(val benchmarkName: String, val url: String) extends TacticTestBase {

  private val EXPORT_DIR = "export" + File.separator

  private val content = DatabasePopulator.loadResource(url)

  it should "export KeYmaera X legacy format" ignore {
    val entries = KeYmaeraXArchiveParser.parse(content, parseTactics = false)
    val printer = new KeYmaeraXLegacyArchivePrinter()
    val printedContent = entries.map(printer(_)).mkString("\n\n")

    // @todo tactic legacy format (so far only id -> closeId renamed manually after export)
    val archive = File(EXPORT_DIR + "legacy")
    archive.createDirectory()
    val pw = (archive / File(url.substring(url.lastIndexOf("/")+1))).printWriter()
    pw.write(printedContent)
    pw.close()
  }

  it should "export KeYmaera X stripped" ignore {
    def stripEntry(e: ParsedArchiveEntry): ParsedArchiveEntry = e.copy(defs = Declaration(Map.empty), tactics = Nil, annotations = Nil)

    val entries = KeYmaeraXArchiveParser.parse(content, parseTactics = false)
    val printer = new KeYmaeraXArchivePrinter()
    val printedStrippedContent = entries.map(stripEntry).map(printer(_)).mkString("\n\n")

    val archive = File(EXPORT_DIR + "stripped" + url.substring(url.lastIndexOf("/")+1))
    archive.createDirectory()
    val pws = archive.printWriter()
    pws.write(printedStrippedContent)
    pws.close()
  }

  it should "export KeYmaera 3 format" in {
    val printer = KeYmaera3PrettyPrinter
    val entries = KeYmaeraXArchiveParser.parse(content, parseTactics = false)
    val printedEntries = entries.map(e =>
      e.name.replaceAll("[ :\\/\\(\\)]", "_") -> (
        try {
          printer.printFile(e.model.asInstanceOf[Formula])
        } catch {
          case ex: Throwable =>
            ex.printStackTrace()
            ""
        })
    )

    val archive = File(EXPORT_DIR + url.substring(url.lastIndexOf("/")+1))
    archive.createDirectory()
    printedEntries.foreach(e => {
      // replace special characters in file name
      val pw = (archive / File(e._1 + ".key")).printWriter()
      pw.write(e._2)
      pw.close()
    })
  }

}

@ExtremeTest
class BenchmarkTester(val benchmarkName: String, val url: String,
                      val timeout: Int, val genCheck: Boolean) extends TacticTestBase with AppendedClues {

  private lazy val entries = {
    println("Reading " + url)
    try {
      DatabasePopulator.readKyx(url)
    } catch {
      case ex: Throwable =>
        println("Failed reading: " + ex.getMessage)
        ex.printStackTrace()
        Nil
    }
  }

  private def tableResults(results: Seq[BenchmarkResult]) = {
    Table(("Benchmark name", "Entry name", "Status", "Duration", "Failure Cause"),
    results.map(r => (benchmarkName, r.name, r.status, r.totalDuration, r.ex)):_*)
  }

  private def setTimeouts(tool: ToolOperationManagement)(testcode: => Any): Unit = {
    withTemporaryConfig(Map(
        Configuration.Keys.QE_ALLOW_INTERPRETED_FNS -> "true",
        Configuration.Keys.ODE_TIMEOUT_FINALQE -> "120",
        Configuration.Keys.Pegasus.INVGEN_TIMEOUT -> "180",
        Configuration.Keys.Pegasus.INVCHECK_TIMEOUT ->"20", // 60
        Configuration.Keys.LOG_QE_DURATION -> "true")) {
      tool.setOperationTimeout(120)
      testcode
    }
  }

  it should "print benchmark index files" ignore withMathematica { _ =>
    val fileName = "./kyx/" + url.substring(url.lastIndexOf("/")+1)
    println(entries.map(e => fileName + "#" + e.name + ";" + timeout + ";0").mkString("\n"))
  }

  it should "prove interactive benchmarks" in withMathematica { tool => setTimeouts(tool) {
    val results = entries.filter(_.name=="").map(e => runInteractive(e.name, e.model, e.tactic.headOption.map(_._2)))
    val writer = new PrintWriter(benchmarkName + "_interactive.csv")
    writer.write(
      "Name,Status,Timeout[min],Duration[ms],Proof Steps,Tactic Size\r\n" + results.map(_.toCsv()).mkString("\r\n"))
    writer.close()
    forEvery(tableResults(results)) { (_, _, status, _, cause) =>
      status should (be("proved") withClue cause or be("skipped"))
    }
  }
  }

  it should "prove interactive benchmarks with Z3" in withZ3 { _ =>
    val results = entries.map(e => runInteractive(e.name, e.model, e.tactic.headOption.map(_._2)))
    val writer = new PrintWriter(benchmarkName + "_interactive_z3.csv")
    writer.write(
      "Name,Status,Timeout[min],Duration[ms],Proof Steps,Tactic Size\r\n" + results.map(_.toCsv()).mkString("\r\n"))
    writer.close()
    forEvery(tableResults(results)) { (_, _, status, _, cause) =>
      status should (be("proved") withClue cause or be("skipped"))
    }
  }

  it should "prove benchmarks with proof hints and Mathematica" in withMathematica { tool => setTimeouts(tool) {
    val results = entries.map(e => runWithHints(e.name, e.model, e.tactic.lastOption.map(_._2)))
    val writer = new PrintWriter(benchmarkName + "_withhints.csv")
    writer.write(
      "Name,Status,Timeout[min],Duration total[ms],Duration QE[ms],Duration gen[ms],Duration check[ms],Proof Steps,Tactic Size\r\n" + results.map(_.toCsv()).mkString("\r\n"))
    writer.close()
    forEvery(tableResults(results)) { (_, name, status, _, cause) =>
      cause match {
        case Some(c) => throw c
        case None =>
          if (entries.find(_.name == name).get.tactic.map(_._2.trim()).contains("master")) status shouldBe "proved"
          else if (status == "proved") fail("Learned how to prove " + name + "; add automated tactic to benchmark")
      }
    }
  }
  }

//  it should "prove benchmarks with proof hints and in Z3" in withZ3 { tool =>
//    setTimeouts(tool)
//    forEvery (entries) { (_, name, modelContent, _) => runWithHints(name, modelContent) }
//  }

  it should "prove benchmarks without proof hints and in Mathematica" in withMathematica { tool => setTimeouts(tool) {
    val results = entries.filter(_.name!="Chinese Train Control System Level 3 (CTCS-3)").map(e => runAuto(e.name, e.model))
    val writer = new PrintWriter(benchmarkName + "_auto.csv")
    writer.write(
      "Name,Status,Timeout[min],Duration total[ms],Duration QE[ms],Duration gen[ms],Duration check[ms],Proof Steps,Tactic Size\r\n" + results.map(_.toCsv()).mkString("\r\n"))
    writer.close()
    forEvery(tableResults(results)) { (_, name, status, _, cause) =>
      if (cause.isDefined) throw cause.get
      else if (entries.find(_.name == name).get.tactic.map(_._2.trim()).contains("master")) status shouldBe "proved"
      else if (status == "proved") fail("Learned how to prove " + name + "; add automated tactic to benchmark")
    }
  }
  }

  it should "generate invariants" ignore withMathematica { tool => setTimeouts(tool) {
    val results = entries.map(e => runInvGen(e.name, e.model))
    val writer = new PrintWriter(benchmarkName + "_invgen.csv")
    writer.write(
      "Name,Status,Timeout[min],Duration total[ms],Duration gen[ms],Duration check[ms],Proof Steps,Tactic Size\r\n" + results.map(_.toCsv()).mkString("\r\n"))
    writer.close()
    if (genCheck) {
      forEvery(tableResults(results)) { (_, name, status, _, cause) =>
        if (entries.find(_.name == name).get.tactic.map(_._2.trim()).contains("master")) status shouldBe "proved" withClue cause
        else if (status == "proved") fail("Learned how to prove " + name + "; add automated tactic to benchmark")
      }
    }
  }
  }

//  it should "prove benchmarks without proof hints and in Z3" in withZ3 { tool =>
//    setTimeouts(tool)
//    forEvery (entries) { (_, name, modelContent, _) => runAuto(name, modelContent) }
//  }

  private def runInteractive(name: String, modelContent: String, tactic: Option[String]) =
    runEntry(name, modelContent, parseWithHints, tactic.map(_.trim) match { case Some("master") => None case t => t })
  private def runWithHints(name: String, modelContent: String, tactic: Option[String]) =
    runEntry(name, modelContent, parseWithHints, Some("master"))
  private def runAuto(name: String, modelContent: String) =
    runEntry(name, modelContent, parseStripHints, Some("master"))

  private def runInvGen(name: String, modelContent: String) = {
    if (genCheck) {
      beforeEach()
      withMathematica(_ => {}) //@HACK beforeEach and afterEach clean up tool provider
      val (model, _) = parseStripHints(modelContent)

      try {
        val Imply(ante, succ) = model
        succ match {
          case Box(_: ODESystem, _) =>
            val seq = Sequent(IndexedSeq(ante), IndexedSeq(succ))
            println(s"Generating invariants $name")
            val invGenStart = System.currentTimeMillis()
            val candidates = InvariantGenerator.pegasusCandidates(seq, SuccPos(0)).toList
            val invGenEnd = System.currentTimeMillis()
            println(s"Done generating (${candidates.map(c => c._1.prettyString + " (proof hint " + c._2 + ")").mkString(",")}) $name")
            if (candidates.nonEmpty) {
              println(s"Checking $name with candidates " + candidates.map(_._1.prettyString).mkString(","))
              TactixLibrary.invSupplier = FixedGenerator(candidates)
              val checkStart = System.currentTimeMillis()
              val proof = proveBy(seq, TactixLibrary.master())
              val checkEnd = System.currentTimeMillis()
              println(s"Done checking $name " + (if (proof.isProved) "(proved)" else "(unfinished)"))

              val result =
                if (proof.isProved) "proved"
                else if (proof.subgoals.exists(s => s.ante.isEmpty && s.succ.size == 1 && s.succ.head == False)) "disproved"
                else "unfinished"
              BenchmarkResult(name, result, timeout, checkEnd - invGenStart,
                qeDurationListener.duration, invGenEnd - invGenStart, checkEnd - checkStart, proof.steps, 1, None)
            } else {
              BenchmarkResult(name, "unfinished (gen)", timeout, invGenEnd - invGenStart, invGenEnd - invGenStart, -1, -1, 0, 1, None)
            }
          case _ =>
            println("Skipping " + name + " for unknown shape, expected [{x'=f(x)}]p(x), but got " + ante.prettyString)
            BenchmarkResult(name, "skipped", timeout, -1, -1, -1, -1, -1, -1, None)
        }
      } catch {
        case ex: TestFailedDueToTimeoutException => BenchmarkResult(name, "timeout", timeout,
          -1, qeDurationListener.duration, -1, -1, -1, -1, Some(ex))
        case ex => BenchmarkResult(name, "failed", timeout, -1, qeDurationListener.duration, -1, -1, -1, -1, Some(ex))
      }
    } else {
      BenchmarkResult(name, "skipped", timeout, -1, -1, -1, -1, -1, -1, None)
    }
  }

  private def runEntry(name: String, modelContent: String, modelParser: String => (Formula, KeYmaeraXArchiveParser.Declaration), tactic: Option[String]): BenchmarkResult = {
    beforeEach()
    //@note connect to mathematica over TCPIP for better control over shutting down kernel
    withTemporaryConfig(Map(Configuration.Keys.MATH_LINK_TCPIP -> "true")) {
      withMathematicaMatlab(_ => {}) //@HACK beforeEach and afterEach clean up tool provider
    }
    val (model, defs) = modelParser(modelContent)
    val result = tactic match {
      case Some(t) =>
        println(s"Proving $name")

        val hasDefinitions = defs.decls.exists(_._2._3.isDefined)
        val tacticExpands = "(expand\\s)|(expandAllDefs)".r.findFirstIn(t).nonEmpty
        if (hasDefinitions) println(s"Example has definitions, auto-expanding at proof start: " + (!tacticExpands))
        val theTactic = BelleParser.parseWithInvGen(t, None, defs, hasDefinitions && !tacticExpands)

        qeDurationListener.reset()
        val start = System.currentTimeMillis()
        try {
          val proof = failAfter(Span(timeout, Seconds))({
            proveBy(model, theTactic)
//            withTacticProgress(theTactic, "_ALL" :: Nil) { proveBy(model, _) }
          })((testThread: Thread) => {
            theInterpreter.kill()
            testThread.interrupt()
          })
          val end = System.currentTimeMillis()
          val result =
            if (proof.isProved) "proved"
            else if (proof.subgoals.exists(s => s.ante.isEmpty && s.succ.size == 1 && s.succ.head == False)) "disproved"
            else "unfinished"
          println(s"Done proving $name: " + result + " in " + (end-start) + "ms")
          BenchmarkResult(name, result, timeout, end - start,
            qeDurationListener.duration, -1, -1, proof.steps, TacticStatistics.size(theTactic), None)
        } catch {
          case ex: TestFailedDueToTimeoutException => BenchmarkResult(name, "timeout", timeout,
            -1, qeDurationListener.duration, -1, -1, -1, -1, Some(ex))
          case ex =>
            ex.printStackTrace()
            BenchmarkResult(name, "failed", timeout, -1, qeDurationListener.duration, -1, -1, -1, -1, Some(ex))
        }
      case None =>
        println("Skipping " + name + " for lack of tactic")
        BenchmarkResult(name, "skipped", timeout, -1, -1, -1, -1, -1, -1, None)
    }
    afterEach()
    result
  }

  /** Parse model and add proof hint annotations to invariant generator. */
  private def parseWithHints(modelContent: String): (Formula, KeYmaeraXArchiveParser.Declaration) = {
    TactixLibrary.invSupplier = FixedGenerator(Nil)
    val generator = new ConfigurableGenerator[GenProduct]()
    KeYmaeraXParser.setAnnotationListener((p: Program, inv: Formula) =>
      generator.products += (p -> (generator.products.getOrElse(p, Nil) :+ (inv, None))))
    val entry = KeYmaeraXArchiveParser(modelContent).head
    TactixLibrary.invSupplier = generator
    TactixLibrary.differentialInvGenerator = InvariantGenerator.cached(InvariantGenerator.differentialInvariantGenerator)
    TactixLibrary.loopInvGenerator = InvariantGenerator.cached(InvariantGenerator.loopInvariantGenerator)
    KeYmaeraXParser.setAnnotationListener((_: Program, _: Formula) => {}) //@note cleanup for separation between tutorial entries
    (entry.model.asInstanceOf[Formula], entry.defs)
  }

  /** Parse model but ignore all proof hints. */
  private def parseStripHints(modelContent: String): (Formula, KeYmaeraXArchiveParser.Declaration) = {
    TactixLibrary.invSupplier = FixedGenerator(Nil)
    TactixLibrary.differentialInvGenerator = InvariantGenerator.cached(InvariantGenerator.differentialInvariantGenerator)
    TactixLibrary.loopInvGenerator = InvariantGenerator.cached(InvariantGenerator.loopInvariantGenerator)
    KeYmaeraXParser.setAnnotationListener((_: Program, _: Formula) => {})
    val entry = KeYmaeraXArchiveParser(modelContent).head
    (entry.model.asInstanceOf[Formula], entry.defs)
  }

}

