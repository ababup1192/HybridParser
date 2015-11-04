package org.ababup1192.parser

import spray.json._

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
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object ObjectNode {
  def newValue(id: Int = -1, kind: String = "object", code: String = "{}", parentId: Int = -1, childrenId: List[Int] = List.empty): ObjectNode = {
    ObjectNode(id, kind, code, parentId, childrenId)
  }

  def fromJson(value: JsValue): Option[ObjectNode] = {
    value.asJsObject.getFields("id", "kind", "code", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(ObjectNode(id.toInt, kind, code, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class EntryNode(id: Int, kind: String, code: String, key: String, parentId: Int, childrenId: List[Int]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "key" -> JsString(key), "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object EntryNode {
  def newValue(id: Int = -1, kind: String = "entry", key: String, parentId: Int = -1, childrenId: List[Int] = List.empty): EntryNode = {
    EntryNode(id, kind, "\"" + key + "\": null", key, parentId, childrenId)
  }

  def fromJson(value: JsValue): Option[EntryNode] = {
    value.asJsObject.getFields("id", "kind", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsString(key), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(EntryNode(id.toInt, kind, code, key, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class ArrayNode(id: Int, kind: String, code: String, parentId: Int, childrenId: List[Int]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object ArrayNode {
  def newValue(id: Int = -1, kind: String = "array", code: String = "[]", parentId: Int = -1, childrenId: List[Int] = List.empty): ArrayNode = {
    ArrayNode(id, kind: String, code, parentId, childrenId)
  }

  def fromJson(value: JsValue): Option[ArrayNode] = {
    value.asJsObject.getFields("id", "kind", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(ArrayNode(id.toInt, kind, code, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class StringNode(id: Int, kind: String, code: String, value: String, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "value" -> JsString(value), "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object StringNode {
  def newValue(id: Int = -1, kind: String = "string", value: String, parentId: Int = -1, childrenId: List[Int] = List.empty): StringNode = {
    StringNode(id, kind, "\"" + value + "\"", "\"" + value + "\"", parentId, childrenId)
  }

  def fromJson(value: JsValue): Option[StringNode] = {
    value.asJsObject.getFields("id", "kind", "value", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsString(value), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(StringNode(id.toInt, kind, code, value, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class NumberNode(id: Int, kind: String, code: String, value: Double, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "value" -> JsNumber(value), "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object NumberNode {
  def newValue(id: Int = -1, kind: String = "number", value: Int, parentId: Int = -1, childrenId: List[Int] = List.empty): NumberNode = {
    if (value.isValidInt) {
      NumberNode(id, kind, value.toInt.toString, value, parentId, childrenId)
    } else {
      NumberNode(id, kind, value.toString, value, parentId, childrenId)
    }
  }

  def fromJson(value: JsValue): Option[NumberNode] = {
    value.asJsObject.getFields("id", "kind", "value", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsNumber(value), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(NumberNode(id.toInt, kind, code, value.toDouble, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean, parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "value" -> JsBoolean(value), "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object BooleanNode {
  def newValue(id: Int = -1, kind: String = "boolean", value: Boolean, parentId: Int = -1, childrenId: List[Int] = List.empty): BooleanNode = {
    BooleanNode(id, kind, value.toString, value, parentId, childrenId)
  }

  def fromJson(value: JsValue): Option[BooleanNode] = {
    value.asJsObject.getFields("id", "kind", "value", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsBoolean(value), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(BooleanNode(id.toInt, kind, code, value, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int, childrenId: List[Int] = List.empty) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind), "code" -> JsString(code),
    "parentId" -> JsNumber(parentId), "childrenId" -> JsArray(childrenId.map(JsNumber(_))))
}

object NullNode {
  def fromJson(value: JsValue): Option[NullNode] = {
    value.asJsObject.getFields("id", "kind", "code", "key", "parentId", "childrenId") match {
      case Seq(JsNumber(id), JsString(kind), JsString(code), JsNumber(parentId), JsArray(childrenId)) =>
        Some(NullNode(id.toInt, kind, code, parentId.toInt, childrenId.map(_.toString.toInt).toList))
      case _ => None
    }
  }
}
