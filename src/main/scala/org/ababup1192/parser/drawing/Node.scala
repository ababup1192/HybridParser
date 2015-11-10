package org.ababup1192.parser.drawing

import org.ababup1192.parser.Fragment

trait Node {
  val id: Int
  val kind: String
  val code: String
  val parentId: Int
  val children: Seq[Node]
  val fragment: Option[Fragment]
}

case class ObjectNode(id: Int, kind: String, code: String, parentId: Int = -1,
                      children: Seq[Node], fragment: Option[Fragment]) extends Node

case class EntryNode(id: Int, kind: String, code: String, key: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node

case class ArrayNode(id: Int, kind: String, code: String, parentId: Int = -1,
                     children: Seq[Node], fragment: Option[Fragment]) extends Node

case class StringNode(id: Int, kind: String, code: String, value: String, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node

case class NumberNode(id: Int, kind: String, code: String, value: Double, parentId: Int = -1,
                      children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean, parentId: Int = -1,
                       children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node

case class NullNode(id: Int, kind: String = "null", code: String = "null", parentId: Int = -1,
                    children: Seq[Node] = Seq.empty, fragment: Option[Fragment]) extends Node
