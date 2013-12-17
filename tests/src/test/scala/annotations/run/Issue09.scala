import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

object C09
@identity @placebo class C09

class Issue09 extends FunSuite {
  test("compiles") {}
}
