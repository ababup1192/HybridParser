package org.ababup1192.hybrid

import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import org.ababup1192.hybrid.util.StringUtil

trait ParserController {
  val parser: Parser

  def setKey(id: Int, newKey: String): Unit

  def setValue(id: Int, value: String): Unit

  def setValue(id: Int, value: Double): Unit

  def setValue(id: Int, value: Boolean): Unit

  def swapArray(from: Int, to: Int): Unit

  def insert(target: Int, value: Any): Unit

  def delete(id: Int): Unit

  def updateCode(newNode: Node): Unit = {
    parser.syntax.getNode(newNode.id).foreach { node =>
      getNodeFragment(node.getId).foreach { fragment =>
        StringUtil.splitString(parser.code, fragment.from.ch, fragment.to.ch) match {
          case (prefix, inner, suffix) =>
            parser.input(prefix + newNode.code + suffix)
        }
      }
    }
  }

  def deleteCode(deleteNode: Node): Unit = {
    parser.syntax.getNode(deleteNode.id).foreach { node =>
      getNodeFragment(node.getId).foreach { fragment =>
        StringUtil.splitString(parser.code, fragment.from.ch, fragment.to.ch) match {
          case (prefix, inner, suffix) =>
            parser.input(prefix + suffix)
        }
      }
    }
  }

  def getNodeFragment(id: Int): Option[Fragment] = {
    parser.syntax.getNode(id) match {
      case Some(node) =>
        Some(Fragment(id, tokenCursor(node.getBegin), tokenCursor(node.getEnd, after = true)))
      case None => None
    }
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false): WordLocation = {
    val (line, ch) = token.collection.cursor(token.index + (if (after) 1 else 0))
    WordLocation(line, ch - 1)
  }
}

case class Fragment(id: Int, from: WordLocation, to: WordLocation)

case class WordLocation(line: Int, ch: Int)





