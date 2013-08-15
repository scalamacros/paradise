package org.scalalang.macroparadise
package reflect

import java.io.{ByteArrayInputStream, File => JFile}
import java.util.UUID.randomUUID
import scala.reflect.io.{AbstractFile, NoAbstractFile, Streamable}

case class EmbeddedFile(val resourcePath: String) extends AbstractFile {
  // compatibility with SBT
  // on the one hand, we need to specify some jfile here, otherwise sbt crashes with an NPE (SI-6870)
  // on the other hand, we can't specify the obvious enclosingUnit, because then sbt somehow gets confused about timestamps
  // okay, now let's specify a guaranteedly non-existent file in an existing directory (so that we don't run into permission problems)
  val name = "embeddedFile-" + resourcePath.replace("/", "-") + "@" + randomUUID().toString.replace("-", "")
  def file = new JFile(name)

  def container = NoAbstractFile
  val path = name
  def absolute = this
  def isDirectory = false
  def iterator = Iterator.empty
  def create() = unsupported()
  def delete() = unsupported()
  def lookupName(name: String, directory: Boolean) = null
  def lookupNameUnchecked(name: String, directory: Boolean) = null
  override def lookupPathUnchecked(path: String, directory: Boolean) = null
  private var _lastModified: Long = 0
  def lastModified: Long = _lastModified
  def lastModified_=(x: Long) = _lastModified = x

  val stream = getClass().getResourceAsStream(resourcePath)
  if (stream == null) throw new Error(s"failed to open an embedded resource from $resourcePath")
  val content = Streamable.bytes(stream)
  override def sizeOption: Option[Int] = Some(content.size)
  def input = new ByteArrayInputStream(content);
  override def output = unsupported
}
