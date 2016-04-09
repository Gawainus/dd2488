package slacc
package lexertests

import org.scalatest.FlatSpec
import utils._
import scala.io.Source
import java.io.File

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
}
