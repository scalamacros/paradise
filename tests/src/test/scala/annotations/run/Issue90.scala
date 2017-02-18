import org.scalatest.FunSuite

import scala.reflect.runtime.{currentMirror => cm}

class Issue90 extends FunSuite {
  test("compileTimeOnly annotation is not added if it is already in the class definition") {
    assert(cm.staticClass("issue90Class").annotations.length == 1)
    assert(cm.staticClass("issue90Class").annotations.head.toString == "scala.annotation.compileTimeOnly(\"this is the only annotation\")")
  }
}
