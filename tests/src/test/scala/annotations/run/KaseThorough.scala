import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

@kase class C[T](x: T, y: Int = 2)(val z: Boolean, val w: String = "")

class KaseThorough extends FunSuite {
  val c = C("42")(true)

  test("construction") {
    assert(c.toString === "C(42,2)")
    assert(new C("42")(true).toString === "C(42,2)")
    assert(c.x === "42")
    assert(c.y === 2)
    assert(c.z === true)
    assert(c.w === "")
  }

  test("deconstruction") {
    val C(x, y) = c
    assert(x === "42")
    assert(y == 2)
  }

  test("copy") {
    val c1 = c.copy(x = 42)(false, "copied")
    assert(c1.toString === "C(42,2)")
    assert(c1.x === 42)
    assert(c1.y === 2)
    assert(c1.z === false)
    assert(c1.w === "copied")
  }

  test("product") {
    assert((c: Product).productPrefix === "C")
    assert(c.productElement(0) === "42")
    assert(c.productElement(1) === 2)
    assert(c.productIterator.toList === List("42", 2))
  }

  test("equality") {
    assert(c === c)
    assert(c != null)
    assert(c === C("42")(true))
    assert(c === C("42")(false, "something different"))
    assert(c != C(43)(true))
  }
}
