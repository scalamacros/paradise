package org.scalalang.macroparadise

object Settings {
  class Setting[T](get: () => T, set: T => Unit) {
    def value = get()
    def value_=(value: T) = set(value)
  }

  def boolSetting(key: String) = new Setting[Boolean](
    get = () => {
      val svalue = System.getProperty("macroparadise." + key)
      svalue != null
    },
    set = value => {
      val svalue = if (value) "true" else null
      System.setProperty("macroparadise." + key, svalue)
    }
  )

  def Yquasiquotedebug = boolSetting("quasiquote.debug")
  def Ydebug = boolSetting("debug")
}