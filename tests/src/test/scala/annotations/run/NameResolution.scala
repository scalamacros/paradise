import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

@identity1 class C1
@pkg.identity2 class C2
@pkg.Module3.identity3 class C3
@Module4.identity4 class C4

class NameResolution extends FunSuite {
  import pkg._
  import Module3._
  import Module4._
  @identity1 class C1
  @identity2 class C2
  @identity3 class C3
  @identity4 class C4

  test("verified at compile-time") {
  }
}

package pkg {
  // @identity1 class C1
  @identity2 class C2
  @Module3.identity3 class C3
  // @Module4.identity4 class C4
}