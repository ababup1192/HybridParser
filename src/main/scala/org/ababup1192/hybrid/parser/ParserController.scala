package org.ababup1192.hybrid.parser

import org.ababup1192.hybrid.util.StringUtil

trait ParserController {
  val parser: Parser

  def setKey(id: Int, newKey: String): Unit

  def setValue(id: Int, value: String): Unit

  def setValue(id: Int, value: Double): Unit

  def setValue(id: Int, value: Boolean): Unit

  def swapArray(from: Int, to: Int): Unit

  def insertNode(newNode: Node): Unit

  def addEntry(target: Int, key: String): Unit

  def addArrayElement(newNode: Node): Unit

  def delete(id: Int): Unit

  /**
   * Update Source Code by using Node
   * @param newNode update target Node
   */
  def updateCode(newNode: Node): Unit = {
    for {
      node <- parser.syntax.getNode(newNode.id)
      fragment <- parser.getNodeFragment(node.getId)
    } {
      val (prefix, _, suffix) = StringUtil.splitString(parser.code, fragment.from, fragment.to)
      parser.input(prefix + newNode.code + suffix)
    }
  }

  /**
   * Delete Source Code by using Node
   * @param deleteNode Target Node of Deleting
   */
  def deleteCode(deleteNode: Node): Unit = {
    for {
      node <- parser.syntax.getNode(deleteNode.id)
      fragment <- parser.getNodeFragment(node.getId)
    } {
      val (prefix, _, suffix) = StringUtil.splitString(parser.code, fragment.from, fragment.to)
      parser.input(prefix + suffix)
    }
  }
}


