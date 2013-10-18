import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class IsoTest extends FunSuite {
  test("main") {
    case class Foo(i: Int, s: String, b: Boolean)
    def foo[C, L](c: C)(implicit iso: Iso[C, L]): L = iso.to(c)

    {
      val equiv = foo(Foo(23, "foo", true))
      def typed[T](t: => T) {}
      typed[(Int, String, Boolean)](equiv)
      println(equiv)
    }
  }
}
