package scala

import scala.reflect.runtime.{universe => ru}

package object quasiquotes {
  val ListOfNil = List(Nil)
  val globalFreshNameCreator = new FreshNameCreator
  val internal: QuasiquoteCompat { val u: ru.type } = QuasiquoteCompat[ru.type](ru)
}