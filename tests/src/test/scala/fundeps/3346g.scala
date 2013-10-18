import org.scalatest.FunSuite
import scala.language.implicitConversions

class Test3346g extends FunSuite {
  test("main") {
    case class A(b: Int, c: String)
    implicit def s2i(s: String): Int = s.length
    implicit def toA[T](t: T)(implicit f: T => Int): A = A(f(t), t.toString)
    assert("asdf".copy(b = 3).toString === "A(3,asdf)")
  }
}