package slacc
package tests

import java.io.{File, FileWriter}

import org.scalatest.FlatSpec
import slacc.ast._
import slacc.lexer._
import slacc.utils._

import scala.io.Source

class TestBed extends FlatSpec {
  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var ctx = Context(reporter = reporter)

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "--print" :: args =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(files = new File(f) :: ctx.files)
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (ctx.files.size != 1) {
      reporter.fatal("Exactly one file expected, " + ctx.files.size + " file(s) given.")
    }

    ctx
  }

  // Parser
  val urlResults = getClass.getResource("/results")
  val pathResults = urlResults.toString.substring(5)
  val dirPathValid = "/givencases/lab3/valid/"

  // Printer
  def executePrinterTest(testFileName: String): Boolean = {
    val urlCase = getClass.getResource(dirPathValid+testFileName)
    val pathStr = urlCase.toString.substring(5)

    // round 1
    val pathPrinter = pathResults + "/printer/"
    val inputs: Array[String] = Array(pathStr, "--print")
    val ctx = processOptions(inputs)
    val pipeline = Lexer andThen Parser
    val ast = pipeline.run(ctx)(ctx.files.head)
    val result: String = Printer(ast)
    val resultPath = pathPrinter + testFileName + ".printer"

    writeResult(resultPath, result)

    // round 2
    val pathPrinter2 = pathResults + "/printer2/"
    val inputs2: Array[String] = Array(resultPath, "--print")
    val ctx2 = processOptions(inputs)
    val pipeline2 = Lexer andThen Parser
    val ast2 = pipeline.run(ctx)(ctx.files.head)
    val result2: String = Printer(ast2)
    val resultPath2 = pathPrinter2 + testFileName + ".printer"

    writeResult(resultPath2, result2)

    val o1 = Source.fromFile(resultPath)
    val o2 = Source.fromFile(resultPath2)
    val isEqual = compareSources(o1, o2)

    o1.close
    o2.close

    isEqual
  }

  def executeAstTest(testFileName: String): Boolean = {
    val urlCase = getClass.getResource(dirPathValid+testFileName)
    val pathStr = urlCase.toString.substring(5)

    val inputs: Array[String] = Array(pathStr, "--ast")
    val ctx = processOptions(inputs)

    val pipeline = Lexer andThen Parser
    val ast = pipeline.run(ctx)(ctx.files.head)

    val result: String = ast.toString

    val resultPath = pathResults + testFileName + ".ast"

    writeResult(resultPath, result)

    val ans = Source.fromFile(pathStr + ".ast")
    val output = Source.fromFile(resultPath)
    val isEqual = compareSources(ans, output)

    ans.close()
    output.close()

    isEqual
  }

  def executeLexerDiyTest(testFileName: String): Boolean = {
    val url = getClass.getResource("/diycases/lexer/"+testFileName)
    val pathStr = url.toString.substring(5)

    val inputs: Array[String] = Array(pathStr)
    val ctx = processOptions(inputs)

    val iter = Lexer.run(ctx)(ctx.files.head)

    while (iter.hasNext) {
      val n = iter.next()
      // println(n+"("+n.line+":"+n.col+")")
      // assert(n.toString == "EOF")
    }
    true
  }

  private def writeResult(resultPath: String, result: String) = {
    val file: File = new File(resultPath)
		val fileWriter: FileWriter = new FileWriter(file)
		fileWriter.write(result)
    fileWriter.write('\n')
		fileWriter.flush()
		fileWriter.close()
  }

  private def compareSources(ans: Source, output: Source): Boolean = {
    val b1: Array[Byte] = ans.map(_.toByte).toArray
    val b2: Array[Byte]  = output.map(_.toByte).toArray
    val isEqual: Boolean = b1.sameElements(b2)
    isEqual
  }
}
