package org.ababup1192.parser.drawing

import org.ababup1192.parser.Fragment
import spray.json._

trait Node {
  val id: Int
  val kind: String
  val code: String
  val parentId: Int
  val children: Seq[Node]
  val fragment: Option[Fragment]

  def toJson: JsValue
}

import org.ababup1192.parser.FragmentProtocol._

case class ObjectNode(id: Int, kind: String, code: String, parentId: Int = -1,
                      children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}


case class EntryNode(id: Int, kind: String, code: String, key: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "key" -> JsString(key) ,"parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

case class ArrayNode(id: Int, kind: String, code: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

case class StringNode(id: Int, kind: String, code: String, value: String, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "value" -> JsString(value), "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

case class NumberNode(id: Int, kind: String, code: String, value: Double, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "value"  -> JsNumber(value), "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean, parentId: Int = -1,
                       children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "value" -> JsBoolean(value) , "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int = -1,
                    children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: JsValue = JsObject("id" -> JsNumber(id), "kind" -> JsString(kind),
    "code" -> JsString(code), "parentId" -> JsNumber(parentId),
    "children" -> this.children.map(_.toJson).toJson, "fragment" -> fragment.map(_.toJson).toJson)
}

