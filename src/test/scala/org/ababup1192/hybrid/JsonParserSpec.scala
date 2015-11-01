package org.ababup1192.hybrid

import org.scalatest._

class JsonParserSpec extends FlatSpec with Matchers {
  "JsonParser" should "parse empty object{}" in {
    val parser = JsonParser()
    parser.input("{}")
    val ast = parser.ast
    ast.length should ===(1)
    ast.headOption === Some(ObjectNode)
  }
}
