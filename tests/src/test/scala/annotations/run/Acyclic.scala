import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class Acyclic extends FunSuite {
  test("A") {
    import acyclica._
    assert(C.toString === "C")
    assert(D.toString === "D")
    assert(new CX().toString === "CX")
    assert(new DX().toString === "DX")
  }
  test("B") {
    import acyclicb._
    assert(CC.x.toString === "DX")
    assert(DD.x.toString === "CX")
  }
  test("C") {
    import Module4._
    @identity4 class C4
  }
}