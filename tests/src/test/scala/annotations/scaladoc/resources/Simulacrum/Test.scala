import scala.language.higherKinds

/**
 */
@simulacrum trait Applicative[F[_]] {
  def pure[A](x: A): F[A]
}