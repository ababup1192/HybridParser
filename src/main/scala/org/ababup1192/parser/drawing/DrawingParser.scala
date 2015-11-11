package org.ababup1192.parser.drawing

trait DrawingParser extends org.ababup1192.parser.Parser {

  def drawingAst: Option[Node] = {
    drawingAst(syntax.getRootNode)
  }

  def drawingAst(node: Option[name.lakhin.eliah.projects.papacarlo.syntax.Node]): Option[Node] = {
    node.map(exportNode(_))
  }

  private def exportNode(node: name.lakhin.eliah.projects.papacarlo.syntax.Node, childrenFlag: Boolean = true): Node = {
    val id = node.getId
    val code = node.sourceCode
    val parentId = node.getParent.map(_.getId).getOrElse(-1)
    val children = if (childrenFlag) {
      node.getBranches.values.flatten.map(exportNode(_, childrenFlag = true)).toList
    } else {
      List.empty[Node]
    }
    val fragment = this.getNodeFragment(id)

    node.getKind match {
      case kind@"object" => ObjectNode(id, kind, code, parentId, children, fragment)
      case kind@"entry" =>
        // Delete left and right double quote
        val key = node.getValues.flatMap(_._2).headOption.getOrElse("").drop(1).dropRight(1)
        EntryNode(id, kind, code, key, parentId, children, fragment)
      case kind@"array" => ArrayNode(id, kind, code, parentId, children, fragment)
      case kind@"string" =>
        val value = code.drop(1).dropRight(1)
        StringNode(id, kind, code, value, parentId, children, fragment)
      case kind@"number" => NumberNode(id, kind, code, code.toDouble, parentId, children, fragment)
      case kind@"boolean" => BooleanNode(id, kind, code, code.toBoolean, parentId, children, fragment)
      case _ => NullNode(id, "null", code, parentId, children, fragment)
    }
  }

}
