package org.ababup1192.parser

import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}

/**
 * Parser has lexer, syntax, AST and ParserController.
 * ParserController can edit Source Code by using AST.
 */
trait Parser {
  val lexer: Lexer
  val syntax: Syntax
  val controller: ParserController
  var addedNodes: List[Int]

  def code: String = {
    syntax.getRootNode.map { root =>
      root.sourceCode
    }.getOrElse("")
  }

  /**
   * Update Source Code and AST
   * @param newCode new Source Code.
   */
  def input(newCode: String): Unit = {
    lexer.input(newCode)
    addedNodes = addedNodes diff (addedNodes diff ast.keys.toList)
  }

  /**
   * Create an another AST for convenience.
   * @return AST ID -> Node
   */
  def ast: Map[Int, Node] = {
    addedNodes.reverse.foldLeft(Map.empty[Int, Node]) { (ast, id) =>
      syntax.getNode(id) match {
        case Some(node) => ast ++ Map(id -> exportNode(node))
        case None => ast
      }
    }
  }

  /**
   * Transform Parser Combinator AST Node -> Another AST Node
   * @param node Syntax AST Node
   * @return Another AST Node
   */
  private def exportNode(node: name.lakhin.eliah.projects.papacarlo.syntax.Node): Node = {
    val id = node.getId
    val code = node.sourceCode
    val parentId = node.getParent.map(_.getId).getOrElse(-1)
    val childrenId = node.getBranches.flatMap(_._2).map(_.getId).toList

    node.getKind match {
      case "object" => ObjectNode(id, code, parentId, childrenId)
      case "entry" =>
        val key = node.getValues.flatMap(_._2).headOption.getOrElse("")
        // Delete left and right double quote
        EntryNode(id, code, key.drop(1).dropRight(1), parentId, childrenId)
      case "array" => ArrayNode(id, code, parentId, childrenId)
      case "string" => StringNode(id, code, value = code, parentId, childrenId)
      case "number" => NumberNode(id, code, code.toDouble, parentId, childrenId)
      case "boolean" => BooleanNode(id, code, code.toBoolean, parentId, childrenId)
      case _ => NullNode(id, code, parentId, childrenId)
    }
  }

  /**
   * Get token index of begin to end.
   * @param id Node ID
   * @return Fragment
   */
  def getNodeFragment(id: Int): Option[Fragment] = {
    syntax.getNode(id) match {
      case Some(node) =>
        Some(Fragment(id, tokenCursor(node.getBegin), tokenCursor(node.getEnd, after = true)))
      case None => None
    }
  }

  def tokenCursor(token: TokenReference, after: Boolean = false): WordLocation = {
    val (line, ch) = token.collection.cursor(token.index + (if (after) 1 else 0))
    WordLocation(line, ch - 1)
  }

}

case class Fragment(id: Int, from: WordLocation, to: WordLocation)

case class WordLocation(line: Int, ch: Int)

