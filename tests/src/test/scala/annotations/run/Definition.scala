import org.scalatest.FunSuite
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class Definition extends FunSuite {
  test("macro annotations get the MACRO flag") {
    assert(cm.staticClass("identity").isMacro === true)
  }
}