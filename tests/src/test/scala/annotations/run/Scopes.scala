import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class Scopes extends FunSuite {
  implicit val x = 42
  @explorer object C

  test("toplevel") {
    assert(A.toString === "can see A, can see B, implicit is <empty>")
    assert(B.toString === "can see A, can see B, implicit is <empty>")
  }

  test("member") {
    assert(C.toString === "can see A, can see B, implicit is <empty>")
  }

  test("local") {
    @explorer object D
    assert(D.toString === "can see A, can see B, implicit is Scopes.this.x")

    {
      val x = 42
      @explorer object E
      assert(E.toString === "can see A, can see B, implicit is Scopes.this.x")

      {
        implicit val x = 42
        @explorer object F
        assert(F.toString === "can see A, can see B, implicit is <empty>")
      }
    }
  }
}
