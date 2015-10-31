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
    parser.ast.find(_.id == id).foreach {
      case node: EntryNode =>
        val newKey = "\"" + key + "\""
        val newCode = node.code.replace(node.key, newKey)
        val newNode = node.copy(key = newKey, code = newCode)
        updateCode(newNode)
      case _ =>
    }
  }

  override def setValue(id: Int, value: String): Unit = {
    parser.ast.find(_.id == id).foreach {
      case node: StringNode =>
        val newValue = "\"" + value + "\""
        val newCode = node.code.replace(node.value, newValue)
        val newNode = node.copy(value = newValue, code = newCode)
        updateCode(newNode)
      case _ =>
    }
  }

  override def setValue(id: Int, value: Double): Unit = {
    parser.ast.find(_.id == id).foreach {
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
    parser.ast.find(_.id == id).foreach {
      case node: BooleanNode =>
        val newNode = node.copy(value = value, code = value.toString)
        updateCode(newNode)
      case _ =>
    }
  }

  override def insert(target: Int, value: Any): Unit = ???

  override def delete(id: Int): Unit = {
    parser.ast.find(_.id == id).foreach { node =>
      val parentNode = parser.ast.find(_.id == node.parentId)
      (parentNode, node) match {
        case (Some(_: EntryNode), _) =>
          val newNode = NullNode(node.id, "null", node.parentId)
          updateCode(newNode)
        case (Some(pNode: ArrayNode), _) =>
          val siblingNodes = parser.ast.filter(n => n.parentId == pNode.id && n.id != node.id)
          val newCode = "[" + siblingNodes.map(_.code).mkString(", ") + "]"
          val newNode = ArrayNode(pNode.id, newCode, pNode.parentId, siblingNodes.map(_.id).toList)
          updateCode(newNode)
        case (Some(pNode: ObjectNode), _: EntryNode) =>
          val siblingNodes = parser.ast.filter(n => n.parentId == pNode.id && n.id != node.id)
          val newCode = "{" + siblingNodes.map(_.code).mkString(", ") + "}"
          val newNode = ArrayNode(pNode.id, newCode, pNode.parentId, siblingNodes.map(_.id).toList)
          updateCode(newNode)
        case (_, _: ObjectNode) =>
          deleteCode(node)
        case _ =>
      }
    }
  }

  override def swapArray(from: Int, to: Int): Unit = ???
}