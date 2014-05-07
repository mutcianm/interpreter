import org.scalatest.FunSuite

class JavaSuite extends FunSuite{
  test("System.getenv test") {
    assert(ctfe { System.getenv().containsKey("JAVA_HOME") })
  }
}
