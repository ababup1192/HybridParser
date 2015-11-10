package org.ababup1192.parser.drawing

import org.ababup1192.parser.Fragment

trait Node {
  val id: Int
  val kind: String
  val code: String
  val parent: Option[Node]
  val children: Seq[Node]
  val fragment: Fragment

  def siblings(node: Node): Seq[Node] = {
    siblingsWithSelf(node).filterNot(_ == node.id)
  }

  def siblingsWithSelf(node: Node): Seq[Node] = {
    node.parent.map { parent =>
      parent.children
    }.getOrElse(Seq.empty[Node])
  }
}

case class ObjectNode(id: Int, kind: String, code: String,
                      parent: Option[Node] = None, children: Seq[Node], fragment: Fragment) extends Node

case class EntryNode(id: Int, kind: String, code: String, key: String,
                     parent: Option[Node] = None, children: Seq[Node], fragment: Fragment) extends Node

case class ArrayNode(id: Int, kind: String, code: String,
                     parent: Option[Node] = None, children: Seq[Node], fragment: Fragment) extends Node

case class StringNode(id: Int, kind: String, code: String, value: String,
                      parent: Option[Node] = None, children: Seq[Node] = Seq.empty, fragment: Fragment) extends Node

case class NumberNode(id: Int, kind: String, code: String, value: Double,
                      parent: Option[Node] = None, children: Seq[Node] = Seq.empty, fragment: Fragment) extends Node

case class BooleanNode(id: Int, kind: String, code: String, value: Boolean,
                       parent: Option[Node] = None, children: Seq[Node] = Seq.empty, fragment: Fragment) extends Node

case class NullNode(id: Int, kind: String = "null", code: String = "null",
                    parent: Option[Node] = None, children: Seq[Node] = Seq.empty, fragment: Fragment) extends Node
