import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class PlaceboAssortedZoo {
  @placebo def foo(x: Int) = x
  @placebo val bar = 2
  @placebo var baz = 3
  @placebo lazy val bax = 4
  @placebo type T = Int
}

class PlaceboAssorted extends FunSuite {
  test("nested") {
    assert(typeOf[PlaceboAssortedZoo].declarations.sorted.map(_.toString).mkString("\n") === """
      |constructor PlaceboAssortedZoo
      |method foo
      |type T
      |value bar
      |value bar
      |method baz
      |method baz_=
      |variable baz
      |value bax
      |lazy value bax
    """.trim.stripMargin)
  }

  test("local") {
    @placebo def foo(x: Int) = x
    @placebo val bar = 2
    @placebo var baz = 3
    @placebo lazy val bax = 4
    @placebo type T = Int

    assert(foo(1) === 1)
    assert(bar === 2)
    assert(baz === 3)
    assert(bax === 4)
    assert(List[T](5) === List(5))
  }
}
