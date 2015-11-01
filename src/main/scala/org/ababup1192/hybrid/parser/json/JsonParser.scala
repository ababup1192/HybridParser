package org.ababup1192.hybrid.parser.json

import name.lakhin.eliah.projects.papacarlo.examples.Json
import org.ababup1192.hybrid.parser._

case class JsonParser() extends Parser {
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

