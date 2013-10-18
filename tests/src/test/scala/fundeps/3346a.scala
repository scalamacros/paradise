import org.scalatest.FunSuite
import scala.language.implicitConversions

class Test3346a extends FunSuite {
  class Rep[T](x : T)

  class SomeOps[T](x : Rep[T]) { def foo = 1 }
  implicit def mkOps[X, T](x : X)(implicit conv: X => Rep[T]) : SomeOps[T] = new SomeOps(conv(x))

  test("main") {
    val a: Rep[Int] = new Rep(42)
    assert(a.foo === 1)
  }
}