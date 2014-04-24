import org.scalatest.FunSuite

class BasicSuite extends FunSuite {

  test("unit value") {
    assert(ctfe { ()  } == ())
  }

  test("evaluates 42") {
    assert(ctfe(42) === 42)
  }

  test("evaluates 8") {
    assert(ctfe({val a = 8; {val a = 1000; 0}; a}) == 8)
  }

  test("mutable values") {
    assert(ctfe({var a = 8; {a = 1000+a; 0}; a}) == 1008)
  }

  test("immutable value scopes") {
    assert(ctfe({val a = 1; {val a = 2; {val a = 3; 0}}; a}) == 1)
  }

  test("primitives' methods evaluation") {
    assert(ctfe({val a = 2; val b = 7; a+b}) == 9)
  }

  test("while test") {
    assert(ctfe({var a = 0; while (a < 10) {a += 1;88}; a}) == 10)
  }

  test("if test") {
    assert(ctfe({val a = 10;  val b = 5; if (a == b+5) "ok" else "notOk"}) == "ok")
  }

  test("value dependency test") {
    assert(ctfe { var a = 99; val b = a; a = 42;  b } == 99)
  }

}