import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class Multiple extends FunSuite {
  @doubler @doubler case object D
  test("multiple") {
    assert(DDDD.toString === "DDDD")
  }
}
