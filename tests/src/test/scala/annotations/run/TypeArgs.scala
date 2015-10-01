import org.scalatest.FunSuite

class TypeArgs extends FunSuite {
  test("macro annotations with type args expand") {
    @shove[Int] val description = "I’m an Int!"
    @shove[String] val bar = "I’m a String!"
    assert(5.description === "I’m an Int!")
    assert("foo".bar === "I’m a String!")
  }
}