package org.ababup1192.hybrid

import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}

trait Parser {
  val lexer: Lexer
  val syntax: Syntax
  val controller: ParserController
  var addedNodes: List[Int]

  def code: String = {
    syntax.getRootNode.map { root =>
      root.sourceCode
    }.getOrElse("")
  }

  def input(newCode: String): Unit = {
    lexer.input(newCode)
  }

  def ast: Vector[org.ababup1192.hybrid.Node] = {
    val ast = addedNodes.reverse.foldLeft(Vector.empty[org.ababup1192.hybrid.Node]) { (vector, id) =>
      syntax.getNode(id) match {
        case Some(node) => vector :+ exportNode(node)
        case None => vector
      }
    }
    ast
  }

  private def exportNode(node: name.lakhin.eliah.projects.papacarlo.syntax.Node): org.ababup1192.hybrid.Node = {
    val id = node.getId
    val code = node.sourceCode
    val parentId = node.getParent.map(_.getId).getOrElse(-1)
    val childrenId = node.getBranches.flatMap(_._2).map(_.getId).toList

    node.getKind match {
      case "object" => ObjectNode(id, code, parentId, childrenId)
      case "entry" =>
        val key = node.getValues.flatMap(_._2).headOption.getOrElse("")
        EntryNode(id, code, key, parentId, childrenId)
      case "array" => ArrayNode(id, code, parentId, childrenId)
      case "string" => StringNode(id, code, value = code, parentId, childrenId)
      case "number" => NumberNode(id, code, code.toDouble, parentId, childrenId)
      case "boolean" => BooleanNode(id, code, code.toBoolean, parentId, childrenId)
      case _ => NullNode(id, code, parentId, childrenId)
    }
  }

}
