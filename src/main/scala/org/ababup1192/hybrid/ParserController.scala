package org.ababup1192.hybrid

trait ParserController {
  val parser: Parser

  def setKey(id: Int, newKey: String): Unit

  def setValue(id: Int, value: String): Unit

  def setValue(id: Int, value: Double): Unit

  def setValue(id: Int, value: Boolean): Unit

  def swapArray(from: Int, to: Int): Unit

  def insert(target: Int, value: Any): Unit

  def delete(id: Int): Unit
}
