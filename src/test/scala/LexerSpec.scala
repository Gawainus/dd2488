package slacc
package tests


class LexerSpec extends TestBed {

  "Test 0: An empty file" should "return only EOF token" in {
    assert(executeLexerDiyTest("test_parser_0_trivial.slac"))
  }

  "Test 1: " should "print some tokens" in {
    assert(executeLexerDiyTest("test_1.slac"))
  }
  "Test 2: " should "print some tokens" in {
    assert(executeLexerDiyTest("test_2.slac"))
  }
  "Test 3: " should "print some tokens" in {
    assert(executeLexerDiyTest("test_3.slac"))
  }
}
