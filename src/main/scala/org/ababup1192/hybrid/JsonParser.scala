package org.ababup1192.hybrid

import name.lakhin.eliah.projects.papacarlo.examples.Json

case class JsonParser() extends Parser {
  val lexer = Json.lexer
  val syntax = Json.syntax(lexer)
  val controller = JsonParserController(this)

  var addedNodes: List[Int] = List(1)

  syntax.onNodeCreate.bind { node =>
    addedNodes = node.getId :: addedNodes
  }


}


case class JsonParserController(parser: JsonParser) extends ParserController {
  override def setKey(id: Int, newKey: String): Unit = ???

  override def setValue(id: Int, value: String): Unit = ???

  override def setValue(id: Int, value: Double): Unit = ???

  override def setValue(id: Int, value: Boolean): Unit = ???

  override def insert(target: Int, value: Any): Unit = ???

  override def delete(id: Int): Unit = ???

  override def swapArray(from: Int, to: Int): Unit = ???
}