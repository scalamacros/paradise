object Test extends App {
  def foo[@identity T]: T @identity = ???
  println(2: @identity)
}