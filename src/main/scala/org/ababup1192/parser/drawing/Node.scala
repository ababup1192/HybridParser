package org.ababup1192.parser.drawing

import org.ababup1192.parser.Fragment
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
    "code" -> Str(code), "value" -> (if (value) True else False), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int = -1,
                    children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node {
  def toJson: Value = Obj("id" -> Num(id), "kind" -> Str(kind),
    "code" -> Str(code), "parentId" -> Num(parentId),
    "children" -> Arr(children.map(_.toJson): _*), "fragment" -> fragment.map(_.toJson).getOrElse(Null))
}

object JsonVisitor {
  def parse(node: Value): Option[Node] = {
    node("kind").value.toString match {
      case "object" =>
        (node("id"), node("kind"), node("code"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Num(parentId), children: Arr, fragment: Obj) =>
            Some(ObjectNode(id.toInt, kind, code, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "entry" =>
        (node("id"), node("kind"), node("code"), node("key"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Str(key), Num(parentId), children: Arr, fragment: Obj) =>
            Some(EntryNode(id.toInt, kind, key, code, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "array" =>
        (node("id"), node("kind"), node("code"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Num(parentId), children: Arr, fragment: Obj) =>
            Some(ArrayNode(id.toInt, kind, code, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "string" =>
        (node("id"), node("kind"), node("code"), node("value"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Str(value), Num(parentId), children: Arr, fragment: Obj) =>
            Some(StringNode(id.toInt, kind, code, value, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "number" =>
        (node("id"), node("kind"), node("code"), node("value"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Num(value), Num(parentId), children: Arr, fragment: Obj) =>
            Some(NumberNode(id.toInt, kind, code, value, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "boolean" =>
        (node("id"), node("kind"), node("code"), node("value"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Num(parentId), value, children: Arr, fragment: Obj) =>
            Some(BooleanNode(id.toInt, kind, code, if (value == True) true else false,
              parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case "null" =>
        (node("id"), node("kind"), node("code"), node("parentId"), node("children"), node("fragment")) match {
          case (Num(id), Str(kind), Str(code), Num(parentId), children: Arr, fragment: Obj) =>
            Some(NullNode(id.toInt, kind, code, parentId.toInt, children.value.flatMap(parse), Fragment.fromJson(fragment)))
          case _ => None
        }
      case _ => None
    }
  }
}
