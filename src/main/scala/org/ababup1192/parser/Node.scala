package org.ababup1192.parser

import org.ababup1192.util.JsonUtil.JsonBoolHelper._
import upickle.Js._

trait Node {
  val id: Int
  val kind: String
  val code: String
  val parentId: Int
  val childrenId: List[Int]

  def siblings(ast: Map[Int, Node]): Vector[Node] = {
    val siblings = ast.get(this.parentId).map { parentNode =>
      parentNode.childrenId.filter(_ != this.id)
    }.getOrElse(List.empty)
    siblings.flatMap(id => ast.get(id)).toVector
  }

  def siblingsWithSelf(ast: Map[Int, Node]): Vector[Node] = {
    val siblings = ast.get(this.parentId).map { parentNode =>
      parentNode.childrenId
    }.getOrElse(List.empty)
    siblings.flatMap(id => ast.get(id)).toVector
  }
}

case class ObjectNode(id: Int, kind: String, code: String, parentId: Int, childrenId: List[Int]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object ObjectNode {
  def newValue(id: Int = -1, kind: String = "object", code: String = "{}", parentId: Int = -1, childrenId: List[Int] = List.empty): ObjectNode = {
    ObjectNode(id, kind, code, parentId, childrenId)
  }
}

case class EntryNode(id: Int, kind: String, code: String, key: String, parentId: Int, childrenId: List[Int]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "key" -> Str(key), "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object EntryNode {
  def newValue(id: Int = -1, kind: String = "entry", key: String, parentId: Int = -1, childrenId: List[Int] = List.empty): EntryNode = {
    EntryNode(id, kind, "\"" + key + "\": null", key, parentId, childrenId)
  }
}

case class ArrayNode(id: Int, kind: String, code: String, parentId: Int, childrenId: List[Int]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object ArrayNode {
  def newValue(id: Int = -1, kind: String = "array", code: String = "[]", parentId: Int = -1, childrenId: List[Int] = List.empty): ArrayNode = {
    ArrayNode(id, kind: String, code, parentId, childrenId)
  }
}

case class StringNode(id: Int, kind: String, code: String, value: String, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "value" -> Str(value), "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object StringNode {
  def newValue(id: Int = -1, kind: String = "string", value: String, parentId: Int = -1, childrenId: List[Int] = List.empty): StringNode = {
    StringNode(id, kind, "\"" + value + "\"", "\"" + value + "\"", parentId, childrenId)
  }
}

case class NumberNode(id: Int, kind: String, code: String, value: Double, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "value" -> Num(value), "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object NumberNode {
  def newValue(id: Int = -1, kind: String = "number", value: Int, parentId: Int = -1, childrenId: List[Int] = List.empty): NumberNode = {
    if (value.isValidInt) {
      NumberNode(id, kind, value.toInt.toString, value, parentId, childrenId)
    } else {
      NumberNode(id, kind, value.toString, value, parentId, childrenId)
    }
  }
}

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "value" -> value.toBool, "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}

object BooleanNode {
  def newValue(id: Int = -1, kind: String = "boolean", value: Boolean, parentId: Int = -1, childrenId: List[Int] = List.empty): BooleanNode = {
    BooleanNode(id, kind, value.toString, value, parentId, childrenId)
  }
}

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind), "code" -> Str(code),
    "parentId" -> Num(parentId), "childrenId" -> Arr(childrenId.map(Num(_)): _*))
}
