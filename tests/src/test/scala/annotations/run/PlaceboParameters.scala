import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class PlaceboParameterZoo {
  class C[@placebo T](@placebo val x: Int)
  object С
  def m[@placebo T, @placebo U](@placebo x: Int)(@placebo y: Int) = ???
  type T[@placebo U] = U
}

class PlaceboParameters extends FunSuite {
  test("combo") {
    assert(typeOf[PlaceboParameterZoo].declarations.sorted.map(_.toString).mkString("\n") === """
      |constructor PlaceboParameterZoo
      |class C
      |object С
      |method m
      |type T
    """.trim.stripMargin)
  }
}
