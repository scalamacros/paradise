import org.scalatest.FunSuite

class Multiple extends FunSuite {
  @doubler @doubler case object D
  test("multiple") {
    assert(DDDD.toString === "DDDD")
  }
}
