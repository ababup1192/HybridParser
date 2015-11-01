package org.ababup1192.hybrid.parser.json

import org.ababup1192.hybrid.parser._

case class JsonParserController(parser: JsonParser) extends ParserController {

  override def setKey(id: Int, key: String): Unit = {
    parser.ast.get(id).foreach {
      case node: EntryNode =>
        val newKey = "\"" + key + "\""
        val newCode = node.code.replace(node.key, newKey)
        val newNode = node.copy(key = newKey, code = newCode)
        updateCode(newNode)
      case _ =>
    }
  }

  override def setValue(id: Int, value: String): Unit = {
    parser.ast.get(id).foreach {
      case node: StringNode =>
        val newValue = "\"" + value + "\""
        val newCode = node.code.replace(node.value, newValue)
        val newNode = node.copy(value = newValue, code = newCode)
        updateCode(newNode)
      case _ =>
    }
  }

  override def setValue(id: Int, value: Double): Unit = {
    parser.ast.get(id).foreach {
      case node: NumberNode =>
        val newNode = if (value.isValidInt) {
          node.copy(value = value, code = value.toInt.toString)
        } else {
          node.copy(value = value, code = value.toString)
        }
        updateCode(newNode)
      case _ =>
    }
  }

  override def setValue(id: Int, value: Boolean): Unit = {
    parser.ast.get(id).foreach {
      case node: BooleanNode =>
        val newNode = node.copy(value = value, code = value.toString)
        updateCode(newNode)
      case _ =>
    }
  }

  /**
   * Insert a node to null node.
   * @param newNode A new node.
   */
  override def insertNode(newNode: Node): Unit = {
    parser.ast.get(newNode.id).foreach {
      case _: NullNode =>
        updateCode(newNode)
    }
  }

  /**
   * Add new entry
   * @param objectId object node ID
   * @param key new entry key
   */
  override def addEntry(objectId: Int, key: String): Unit = {
    val ast = parser.ast
    ast.get(objectId).foreach {
      case objectNode: ObjectNode =>
        val newNode = EntryNode(-1, "\"" + key + "\": null", key, objectNode.id, List.empty)
        val siblingsNodes = newNode.siblings(ast) :+ newNode
        // Rewrite an object code.
        val newCode = "{" + siblingsNodes.map(_.code).mkString(", ") + "}"
        val newObjectNode = objectNode.copy(code = newCode, childrenId = siblingsNodes.map(_.id).toList)
        updateCode(newObjectNode)
    }
  }

  /**
   * Add new element of Array
   * @param newNode an any node must have parent ID that is array node
   */
  override def addArrayElement(newNode: Node): Unit = {
    val ast = parser.ast
    parser.ast.get(newNode.parentId).foreach {
      case arrayNode: ArrayNode =>
        val siblingsNodes = newNode.siblings(ast) :+ newNode
        // Rewrite an array code.
        val newCode = "[" + siblingsNodes.map(_.code).mkString(", ") + "]"
        val newArrayNode = arrayNode.copy(code = newCode, childrenId = siblingsNodes.map(_.id).toList)
        updateCode(newArrayNode)
    }
  }

  /**
   * Swap array element
   * @param from array element node id
   * @param to array element node id
   */
  override def swapArray(from: Int, to: Int): Unit = {
    val ast = parser.ast
    for {
      fromNode <- ast.get(from)
      toNode <- ast.get(to)
      parentNode <- ast.get(toNode.parentId)
    } {
      if (fromNode.parentId == toNode.parentId && parentNode.id == fromNode.parentId) {
        parentNode match {
          case arrayNode: ArrayNode =>
            val fromIndex = arrayNode.childrenId.indexOf(fromNode.id)
            val toIndex = arrayNode.childrenId.indexOf(toNode.id)
            val newChildrenId = arrayNode.childrenId.updated(fromIndex, toNode.id).updated(toIndex, fromNode.id)
            val newChildrenNodes = newChildrenId.flatMap(id => ast.get(id))
            // Rewrite an array code.
            val newCode = "[" + newChildrenNodes.map(_.code).mkString(", ") + "]"
            val newArrayNode = arrayNode.copy(code = newCode, childrenId = newChildrenId)
            updateCode(newArrayNode)
          case _ =>
        }
      }
    }
  }

  override def delete(id: Int): Unit = {
    val ast = parser.ast
    for {
      deleteNode <- ast.get(id)
      deleteParentNode <- ast.get(deleteNode.parentId)
    } {
      (deleteParentNode, deleteNode) match {
        // "SomeEntry": DeleteNode -> "SomeEntry": null
        case (_: EntryNode, _) =>
          val nullNode = NullNode(deleteNode.id, "null", deleteNode.parentId)
          updateCode(nullNode)
        // [a, b, c, deleteNode] -> [a, b, c]
        case (arrayNode: ArrayNode, _) =>
          deleteArrayElement(ast, deleteNode, arrayNode)
        // {"foo": foo, "bar": bar, "DeleteEntry": delete} -> {"foo": foo, "bar": bar}
        case (objectNode: ObjectNode, _: EntryNode) =>
          deleteObjectEntry(ast, deleteNode, objectNode)
        // ObjectNode
        case (_, _: ObjectNode) =>
          deleteCode(deleteNode)
        case _ =>
      }
    }
  }

  private def deleteArrayElement(ast: Map[Int, Node], deleteNode: Node, arrayNode: ArrayNode): Unit = {
    val siblingsNodes = deleteNode.siblingsWithSelf(ast)
    val newCode = "[" + siblingsNodes.map(_.code).mkString(", ") + "]"
    val newNode = ArrayNode(arrayNode.id, newCode, arrayNode.parentId, siblingsNodes.map(_.id).toList)
    updateCode(newNode)
  }

  private def deleteObjectEntry(ast: Map[Int, Node], deleteEntry: Node, objectNode: ObjectNode): Unit = {
    val siblingsNodes = deleteEntry.siblingsWithSelf(ast)
    val newCode = "{" + siblingsNodes.map(_.code).mkString(", ") + "}"
    val newNode = ArrayNode(objectNode.id, newCode, objectNode.parentId, siblingsNodes.map(_.id).toList)
    updateCode(newNode)
  }

}