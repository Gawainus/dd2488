package slacc
package tests


class PrinterSpec extends TestBed {


  "Test 0: 99bottles" should "contain the same content" in {

    val testFileName = "/99bottles.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 1: BinarySearch" should "contain the same content" in {

    val testFileName = "/BinarySearch.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 2: Calendar" should "contain the same content" in {

    val testFileName = "/Calendar.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 3: ComplexNumbers" should "contain the same content" in {

    val testFileName = "/ComplexNumbers.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 4: DrawStuff" should "contain the same content" in {

    val testFileName = "/DrawStuff.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 5: Factorial" should "contain the same content" in {

    val testFileName = "/Factorial.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 7: GCD" should "contain the same content" in {

    val testFileName = "/GCD.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 8: HeapSort" should "contain the same content" in {

    val testFileName = "/HeapSort.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 9: Life" should "contain the same content" in {

    val testFileName = "/Life.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 10: Multiplicator" should "contain the same content" in {

    val testFileName = "/Multiplicator.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 11: NewtonsMethod" should "contain the same content" in {

    val testFileName = "/NewtonsMethod.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 12: OptimalChange" should "contain the same content" in {

    val testFileName = "/OptimalChange.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 13: Polymorphism" should "contain the same content" in {

    val testFileName = "/Polymorphism.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 14: PrimeTest" should "contain the same content" in {

    val testFileName = "/PrimeTest.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 15: QuickSort" should "contain the same content" in {

    val testFileName = "/QuickSort.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 16: ScalarProduct" should "contain the same content" in {

    val testFileName = "/ScalarProduct.slac"
    assert(executePrinterTest(testFileName))
  }
  "Test 17: Simple" should "contain the same content" in {

    val testFileName = "/Simple.slac"
    assert(executePrinterTest(testFileName))
  }
  "Test 18: Sudoku" should "contain the same content" in {

    val testFileName = "/Sudoku.slac"
    assert(executePrinterTest(testFileName))
  }

  "Test 19: VehicleRent" should "contain the same content" in {

    val testFileName = "/VehicleRent.slac"
    assert(executePrinterTest(testFileName))
  }

}
