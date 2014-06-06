import org.scalatest.FunSuite

class ExceptionSuite extends FunSuite {

  test("exception simple") {
    assert(ctfe {
      class A extends Throwable
      try {
        throw new A
      } catch {
        case _:A           => 42
        case _:Throwable   => 999
      }
    } == 42)
  }

  test("exception in a function call") {
    assert(ctfe {
      class A extends Throwable
      def f = {throw new A; 999}
      try { f } catch {
        case _:A           => 42
        case _:Throwable   => 888
      }
    } == 42)
  }

  test("exception in a nested function call") {
    assert(ctfe {
      class A extends Throwable
      def f = {throw new A; 999}
      def g = {f; 777}
      try { g } catch {
        case _:A           => 42
        case _:Throwable   => 888
      }
    } == 42)
  }

  test("exception caught by Throwable") {
    assert(ctfe {
      class A extends Throwable
      class B extends Throwable
      def f = {throw new A; 999}
      def g = {f; 777}
      try { g } catch {
        case _:B           => 888
        case _:Throwable   => 42
      }
    } == 42)
  }

}
