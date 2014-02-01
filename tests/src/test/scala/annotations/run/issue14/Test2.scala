package issue14

trait P2[T] { def foo: T }
object test2 {
  class PWrapper[T] {
    import java.util // make sure that macro expansion logic skips import contexts
    import java.lang.reflect
    @pkg.happytee val self: P2[T] = ???
  }
}