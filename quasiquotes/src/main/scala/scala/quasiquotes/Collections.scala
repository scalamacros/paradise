package scala.quasiquotes

object Collections {
  final def mmap[A, B](xss: List[List[A]])(f: A => B) = xss map (_ map f)
  final def foreach2[A, B](xs1: List[A], xs2: List[B])(f: (A, B) => Unit): Unit = {
    var ys1 = xs1
    var ys2 = xs2
    while (!ys1.isEmpty && !ys2.isEmpty) {
      f(ys1.head, ys2.head)
      ys1 = ys1.tail
      ys2 = ys2.tail
    }
  }
}