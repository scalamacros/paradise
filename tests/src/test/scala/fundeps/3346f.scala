import org.scalatest.FunSuite
import scala.language.implicitConversions
import scala.language.reflectiveCalls

class Test3346f extends FunSuite {
  trait Foo[A]
  implicit def fooString: Foo[String] = null
  implicit def value[A](implicit foo: Foo[A]) = 5

  implicit def conversion[A](x: Int)(implicit foo: Foo[A]) = new {
    def aMethod = 5
  }

  test("main") {
    assert(implicitly[Int] === 5)
    assert(1.aMethod === 5)
  }
}