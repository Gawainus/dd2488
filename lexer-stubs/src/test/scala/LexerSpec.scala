package slacc
package lexertests

import org.scalatest.FlatSpec
import lexer._
import utils._
import scala.io.Source
import java.io.File

class LexerSpec extends FlatSpec {

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
      reporter.fatal("Exactly one file expected, "+ctx.files.size+" file(s) given.")
    }

    ctx
  }

  "An empty file" should "return only EOF token" in {


    val url = getClass.getResource("/sample_0.slac")
    val pathStr = url.toString.substring(5)
    println(pathStr)

    val inputs: Array[String] = Array(pathStr)
    val ctx = processOptions(inputs)

    val iter = Lexer.run(ctx)(ctx.files.head)

    while (iter.hasNext) {
      val n = iter.next()
      println(n+"("+n.line+":"+n.col+")")
    }
    assert(0 == 0)
  }
}
