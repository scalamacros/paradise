import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class DefinitionTests extends FunSuite {
  val identity = typeOf[identity].typeSymbol

  test("macro annotations get the MACRO flag") {
    assert(identity.isMacro === true)
  }
}