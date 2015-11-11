package org.ababup1192.parser.drawing

import org.ababup1192.parser.Fragment
import org.ababup1192.util.JsonUtil.JsonBoolHelper._
import upickle.Js._

trait Node {
  val id: Int
  val kind: String
  val code: String
  val parentId: Int
  val children: Seq[Node]
  val fragment: Option[Fragment]

  def toJson: Value
}

case class ObjectNode(id: Int, kind: String, code: String, parentId: Int = -1,
                      children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}


case class EntryNode(id: Int, kind: String, code: String, key: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "key" -> Str(key), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class ArrayNode(id: Int, kind: String, code: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class StringNode(id: Int, kind: String, code: String, value: String, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "value" -> Str(value), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class NumberNode(id: Int, kind: String, code: String, value: Double, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "value" -> Num(value), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean, parentId: Int = -1,
                       children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "value" -> value.toBool, "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int = -1,
                    children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}
