package slacc
package lexertests

import lexer._

class LexerSpec extends TestBed {

  val lexerTestPath = "/lexer_tests/"

  "Lexer Test 0: An empty file" should "return only EOF token" in {
    val url = getClass.getResource(lexerTestPath + "test_0.slac")
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


  "Lexer Test 1: A file with comment" should "return CMT token" in {
    val url = getClass.getResource(lexerTestPath + "test_1.slac")
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

  "Lexer Test 2: A class declaration" should "match tokens" in {
    val url = getClass.getResource(lexerTestPath + "test_2.slac")
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
