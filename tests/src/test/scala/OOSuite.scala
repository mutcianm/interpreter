import org.scalatest.FunSuite

class OOSuite extends FunSuite {

  test("trait method lookup simple") {
    assert(ctfe { trait A { def f() = 42 }; object B extends A; B.f() } == 42)
  }

  test("inherited method call") {
    assert(ctfe { class A {def f() = 42}; class B extends A; (new B).f()} == 42)
  }

  test("inherited field access") {
    assert(ctfe { class A {val v = 42}; class B extends A; (new B).v } == 42)
  }

  test("multiple field inheritance") {
    assert(ctfe {
      trait A{def a = 10}
      trait B{def b = 30}
      trait C{def c = 2}
      class F extends A with B with C {def f = a+b+c}
      (new F).f
    } == 42)
  }

  test("mutable fields") {
    assert(ctfe {
      class A {var v = 100}
      val a = new A
      a.v = 42
      a.v
    } == 42)
  }

  test("runtime polymorphism simple") {
    assert(ctfe { class A{def f() = 100}; class B extends A{def f() = 42}; (new B).f() } == 42)
  }

  test("simple super call") {
    assert(ctfe { class A{def f() = 100}; class B extends A{def f() = super.f()+42}; (new B).f() } == 142)
  }

  test("polymorphic method call") {
    assert(ctfe {
      class A { def f() = 100}
      class B extends A { def f() = 42}
      val b: A = new B
      b.f()
    } == 42)
  }

  test("polymorphic val") {
    assert(ctfe {
      class A { val v = 99}
      class B extends A { val v = 42}
      val b: A = new B
      b.v
    } == 42)
  }

  test("runtime polymorphism with a callback") {
    assert(ctfe {
      class A { def f() = 999; def g() = f() + 2}
      class B extends A {override def f() = 40}
      (new B).g()
    } == 42)
  }

  test("constructor simple") {
    assert(ctfe { class A(val v: Int); new A(42).v } == 42 )
  }

  test("constructor initialization order") {
    assert(ctfe { class A(var v: Int){v = 42}; new A(999).v } == 42)
  }

  test("multiple parameter list cons") {
    assert(ctfe { class A(a:Int)(b:Int){def f = a+b}; new A(40)(2).f } == 42)
  }

}
