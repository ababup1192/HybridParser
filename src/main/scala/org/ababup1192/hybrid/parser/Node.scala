package org.ababup1192.hybrid.parser

trait Node {
  val id: Int
  val code: String
  val parentId: Int
  val childrenId: List[Int]

  def siblings(ast: Map[Int, Node]): Vector[Node] = {
    ast.values.par.filter(node => node.parentId == this.parentId).toVector
  }

  def siblingsWithSelf(ast: Map[Int, Node]): Vector[Node] = {
    ast.values.par.filter(node => node.parentId == this.parentId && this.id != node.id).toVector
  }
}

case class ObjectNode(id: Int, code: String, parentId: Int, childrenId: List[Int]) extends Node

object ObjectNode {
  def newValue(id: Int = -1, code: String = "{}", parentId: Int = -1, childrenId: List[Int] = List.empty): ObjectNode = {
    ObjectNode(id, code, parentId, childrenId)
  }
}

case class EntryNode(id: Int, code: String, key: String, parentId: Int, childrenId: List[Int]) extends Node

case class ArrayNode(id: Int, code: String, parentId: Int, childrenId: List[Int]) extends Node

case class StringNode(id: Int, code: String, value: String, parentId: Int, childrenId: List[Int]) extends Node

case class NumberNode(id: Int, code: String, value: Double, parentId: Int, childrenId: List[Int]) extends Node

object NumberNode {
  def newValue(id: Int = -1, value: Int, parentId: Int = -1, childrenId: List[Int] = List.empty): NumberNode = {
    if (value.isValidInt) {
      NumberNode(id, value.toInt.toString, value, parentId, childrenId)
    } else {
      NumberNode(id, value.toString, value, parentId, childrenId)
    }
  }
}

case class BooleanNode(id: Int, code: String, value: Boolean, parentId: Int, childrenId: List[Int]) extends Node

case class NullNode(id: Int, code: String = "null", parentId: Int, childrenId: List[Int] = List.empty) extends Node




