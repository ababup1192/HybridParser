package org.ababup1192.hybrid

trait Node {
  val id: Int
  val code: String
  val parentId: Int
  val childrenId: List[Int]
}

case class ObjectNode(id: Int, code: String, parentId: Int, childrenId: List[Int]) extends Node

case class EntryNode(id: Int, code: String, key: String, parentId: Int, childrenId: List[Int]) extends Node

case class ArrayNode(id: Int, code: String, parentId: Int, childrenId: List[Int]) extends Node

case class StringNode(id: Int, code: String, value: String, parentId: Int, childrenId: List[Int]) extends Node

case class NumberNode(id: Int, code: String, value: Double, parentId: Int, childrenId: List[Int]) extends Node

case class BooleanNode(id: Int, code: String, value: Boolean, parentId: Int, childrenId: List[Int]) extends Node

case class NullNode(id: Int, code: String, parentId: Int, childrenId: List[Int]) extends Node




