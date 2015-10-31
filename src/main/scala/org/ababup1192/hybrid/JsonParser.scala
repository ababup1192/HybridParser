package org.ababup1192.hybrid

import name.lakhin.eliah.projects.papacarlo.examples.Json

case class JsonParser() extends Parser {
  val lexer = Json.lexer
  val syntax = Json.syntax(lexer)

  var addedNodes: List[Int] = List(1)

  syntax.onNodeCreate.bind { node =>
    addedNodes = node.getId :: addedNodes
  }


}
