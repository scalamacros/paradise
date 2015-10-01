import org.scalatest.FunSuite

@identity case class InteropIdentity(x: Int)
@placebo case class InteropPlacebo(x: Int)

class InteropCaseSynthesis extends FunSuite {
  test("case module synthesis for identity") {
    assert(InteropIdentity.toString === "InteropIdentity")
  }

  test("case module synthesis for placebo") {
    assert(InteropPlacebo.toString === "InteropPlacebo")
  }
}