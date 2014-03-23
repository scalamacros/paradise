package scala.quasiquotes

trait StandardLiftables { self =>
  val u: scala.reflect.api.Universe
  // NOTE: can't compile this file as part of the plugin
  // because that would lead to bootstrapping problems
}
