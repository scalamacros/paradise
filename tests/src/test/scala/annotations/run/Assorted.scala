import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class AssortedZoo {
  @doubler def foo(x: Int) = x
  @doubler val bar = 2
  @doubler var baz = 3
  @doubler lazy val bax = 4
  @doubler type T = Int
}

class Assorted extends FunSuite {
  test("nested") {
    assert(typeOf[AssortedZoo].decls.sorted.map(_.toString).mkString("\n") === """
      |constructor AssortedZoo
      |method foofoo
      |value barbar
      |value barbar
      |method bazbaz
      |method bazbaz_=
      |variable bazbaz
      |value baxbax
      |lazy value baxbax
      |type TT
    """.trim.stripMargin)
  }

  test("local") {
    @doubler def foo(x: Int) = x
    @doubler val bar = 2
    @doubler var baz = 3
    @doubler lazy val bax = 4
    @doubler type T = Int

    assert(foofoo(1) === 1)
    assert(barbar === 2)
    assert(bazbaz === 3)
    assert(baxbax === 4)
    assert(List[TT](5) === List(5))
  }
}
