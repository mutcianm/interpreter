import org.scalatest.FunSuite

class ModuleSuite extends FunSuite{

  test("object declaration") {
    assert(ctfe { object A {}; 0} == 0)
  }

  test("object method") {
    assert(ctfe { object A { def f() = 42}; A.f() } == 42)
  }

  test("object methods") {
    assert(ctfe { object A { def f() = g(); def g() = 42}; A.f() } == 42 )
  }

  test("object fields") {
    assert(ctfe { object A { val v = 42}; A.v } == 42)
  }

  test("object mutable fields") {
    assert(ctfe { object A {var v = 0}; A.v = 42; A.v } == 42)
  }

  test("nested object declaration") {
    assert(ctfe { object A { object B {}}; 0 } == 0)
  }

  test("nested object field access") {
    assert(ctfe { object A {object B {val c = 42}}; A.B.c } == 42)
  }

  test("nested object method access") {
    assert(ctfe { object A { object B { def foo() = 42}}; A.B.foo() } == 42)
  }

  test("object value capturing") {
    assert(ctfe { val a = 20; object A { val b = 2; object B {def foo(c: Int) = a+b+c}}; A.B.foo(20)} == 42 )
  }

  test("object lazy initialization") {
    assert(ctfe { var a = 40; object A { a = 2; val b = 80}; val c = a; A.b-c+a } == 42 )
  }

  test("object transitive immutability") {
    assert(ctfe { object A { var a = 42}; var v = A.a; v = 100; A.a } == 42)
  }

  test("module forward reference") {
    assert(ctfe { object A {def f = 42}; object B {def g = A.f}; B.g} == 42)
  }

  test("module cross field references") {
    assert(ctfe {
      object A {
        val a = 100
        val b = a
        val c = b - a + 42
      }
      A.c
    } == 42)
  }

  test("object lazy fields") {
    assert(ctfe {
      object A {
        var v = 100
        lazy val a = v
        v = 42
      }
      A.a
    } == 42)
  }

  test("module reinitialization") {
    assert(ctfe {
      object A {var v = 888}
      def f() = A.v = 42
      f()
      A.v
    } == 42)
  }

}
