import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

package object pkg1 {
  @doubler def foo(x: Int) = x
  @doubler val bar = 2
  @doubler var baz = 3
  @doubler lazy val bax = 4
  @doubler type T = Int
}

package pkg2 {
  @hello object `package`
}

class PackageObject extends FunSuite {
  test("package object members") {
    import pkg1._
    assert(foofoo(1) === 1)
    assert(barbar === 2)
    assert(bazbaz === 3)
    assert(baxbax === 4)
    assert(List[TT](5) === List(5))
  }

  test("package object itself") {
    assert(pkg2.hello === "hello")
  }
}
