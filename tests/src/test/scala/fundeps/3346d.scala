import org.scalatest.FunSuite
import scala.language.implicitConversions

class Test3346d extends FunSuite {
  trait TARInt

  trait Basket[A,B] {
    def iAmABasket = {}
  }

  trait BasketFactory[A,B] {
    def create(v: A): Basket[A,B]
  }

  implicit val bf = new BasketFactory[Int,TARInt] {
    def create(v: Int): Basket[Int,TARInt] = new Basket[Int, TARInt]{}
  }

  implicit def i2[A,B](a: A)(implicit bf: BasketFactory[A,B]): Basket[A,B] = bf.create(a)

  test("main") {
    1.iAmABasket // <-- i2 conversion not applicable
  }
}