package org.ababup1192.parser.json

import name.lakhin.eliah.projects.papacarlo.examples.Json
import org.ababup1192.parser.drawing.DrawingParser

case class JsonParser() extends DrawingParser {
  val lexer = Json.lexer
  val syntax = Json.syntax(lexer)
  val controller = JsonParserController(this)

  // List of added node ID
  // Initialize List that add Root node
  var addedNodes: List[Int] = List(1)

  syntax.onNodeCreate.bind { node =>
    addedNodes = node.getId :: addedNodes
  }
}

