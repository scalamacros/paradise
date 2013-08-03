object Test extends App {
  type T1 = List[T] forSome { @identity type T <: Int }
  type T2 = List[Int] { @identity def head: Int }
}