package org.ababup1192.hybrid

import name.lakhin.eliah.projects.papacarlo.examples.Json

case class JsonParser() extends Parser {
  val lexer = Json.lexer
  val syntax = Json.syntax(lexer)
  val controller = JsonParserController(this)

  var addedNodes: List[Int] = List(1)

  syntax.onNodeCreate.bind { node =>
    addedNodes = node.getId :: addedNodes
  }
}

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
        updateCode(if (value.isValidInt) {
          node.copy(value = value, code = value.toInt.toString)
        } else {
          node.copy(value = value, code = value.toString)
        })
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

  override def insertNode(newNode: Node): Unit = {
    parser.ast.get(newNode.id).foreach {
      case _: NullNode =>
        updateCode(newNode)
    }
  }

  override def addEntry(target: Int, key: String): Unit = {
    val ast = parser.ast
    ast.get(target).foreach {
      case targetNode: ObjectNode =>
        val newNode = EntryNode(-1, "\"" + key + "\": null", key, target, List.empty)
        val siblingsNodes = newNode.siblings(ast) :+ newNode
        val newCode = "{" + siblingsNodes.map(_.code).mkString(", ") + "}"
        val newObjectNode = ObjectNode(targetNode.id, newCode, targetNode.parentId, siblingsNodes.map(_.id).toList)
        updateCode(newObjectNode)
    }
  }

  override def addArrayElement(newNode: Node): Unit = {
    val ast = parser.ast
    parser.ast.get(newNode.parentId).foreach {
      case targetNode: ArrayNode =>
        val siblingsNodes = newNode.siblings(ast) :+ newNode
        val newCode = "[" + siblingsNodes.map(_.code).mkString(", ") + "]"
        val newArrayNode = ArrayNode(targetNode.id, newCode, targetNode.parentId, siblingsNodes.map(_.id).toList)
        updateCode(newArrayNode)
    }
  }

  override def swapArray(from: Int, to: Int): Unit = {
    val ast = parser.ast
    for {
      fromNode <- ast.get(from)
      toNode <- ast.get(to)
      parentNode <- ast.get(toNode.parentId)
    } {
      if (fromNode.parentId == toNode.parentId && parentNode.id == fromNode.parentId) {
        parentNode match {
          case _: ArrayNode =>
            val fromIndex = parentNode.childrenId.indexOf(fromNode.id)
            val toIndex = parentNode.childrenId.indexOf(toNode.id)
            val newChildrenId = parentNode.childrenId.updated(fromIndex, toNode.id).updated(toIndex, fromNode.id)
            val newChildrenNodes = newChildrenId.flatMap(id => ast.get(id))
            val newCode = "[" + newChildrenNodes.map(_.code).mkString(", ") + "]"
            val newNode = ArrayNode(parentNode.id, newCode, parentNode.parentId, newChildrenId)
            updateCode(newNode)
          case _ =>
        }
      }
    }
  }

  override def delete(id: Int): Unit = {
    val ast = parser.ast
    ast.get(id).foreach { node =>
      val parentNode = ast.get(node.parentId)
      (parentNode, node) match {
        case (Some(_: EntryNode), _) =>
          val newNode = NullNode(node.id, "null", node.parentId)
          updateCode(newNode)
        case (Some(pNode: ArrayNode), _) =>
          val siblingsNodes = node.siblingsWithSelf(ast)
          val newCode = "[" + siblingsNodes.map(_.code).mkString(", ") + "]"
          val newNode = ArrayNode(pNode.id, newCode, pNode.parentId, siblingsNodes.map(_.id).toList)
          updateCode(newNode)
        case (Some(pNode: ObjectNode), _: EntryNode) =>
          val siblingsNodes = node.siblingsWithSelf(ast)
          val newCode = "{" + siblingsNodes.map(_.code).mkString(", ") + "}"
          val newNode = ArrayNode(pNode.id, newCode, pNode.parentId, siblingsNodes.map(_.id).toList)
          updateCode(newNode)
        case (_, _: ObjectNode) =>
          deleteCode(node)
        case _ =>
      }
    }
  }

}