import org.scalatest.FunSuite

class PrimitivesSuite extends FunSuite {
  // TODO: all supported methods should be covered

  test("Int + Int") {
    assert(ctfe { val a = 100; val b = 1; a+b } == 101)
  }

  test("Int == Int") {
    assert(ctfe { val a = 1; val b = 1; a == b })
  }

  test("Int < Int") {
    assert(ctfe { val a = 100; val b = 99; b < a })
  }

  test("Double + Double") {
    assert(ctfe {
      val a = 42.2d
      val b = 0.04d
      a+b
    } == 42.24)
  }

  test("toChar") { assert(ctfe { 42.toChar.isInstanceOf[Char] }) }

  test("toByte") { assert(ctfe { 42.toByte.isInstanceOf[Byte] }) }

  test("toShort") { assert(ctfe { 42.toShort.isInstanceOf[Short] }) }

  test("toInt") { assert(ctfe { 42.0d.toInt.isInstanceOf[Int] }) }

  test("toLong") { assert(ctfe { 42.toLong.isInstanceOf[Long] }) }

  test("toFloat") { assert(ctfe { 42.toFloat.isInstanceOf[Float] }) }

  test("toDouble") { assert(ctfe { 42.toDouble.isInstanceOf[Double] }) }

  test("Byte -> Int widening") {
    assert(ctfe { 
      val a = 2.toByte
      val b = 40
      (a+b).isInstanceOf[Int]
    })
  }

  test("Short -> Int widening") {
    assert(ctfe {
      val a = 40.toShort
      val b = 2
      (a+b).isInstanceOf[Int]
    })
  }

  test("Int -> Long widening") {
    assert(ctfe {
      val a = 40
      val b = 2L
      (a+b).isInstanceOf[Long]
    })
  }

  test("Int -> Double widening") {
    assert(ctfe {
      val a = 40
      val b = 2.0d
      (a+b).isInstanceOf[Double]
    })
  }

  test("Float -> Double widening") {
    assert(ctfe {
      val a = 2.0
      val b = 40.0d
      (a+b).isInstanceOf[Double]
    })
  }

  test("Byte integral overflow") {
    assert(ctfe {
      val a = 255.toByte
      val b = 215.toByte
      a+b
    } == -42)
  }

}
