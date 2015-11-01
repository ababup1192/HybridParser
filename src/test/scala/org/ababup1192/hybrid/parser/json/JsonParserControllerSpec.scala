package org.ababup1192.hybrid.parser.json

import org.ababup1192.hybrid.parser._
import org.scalatest._

class JsonParserControllerSpec extends FlatSpec with Matchers {

  "JsonParserController" should "set entry node key" in {
    val parser = JsonParser()
    parser.input( """{"foo": 123}""")
    parser.controller.setKey(2, "bar")
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(4).foreach {
      case node: EntryNode =>
        parser.code should ===( """{"bar": 123}""")
        node.key should ===("\"" + "bar" + "\"")
        node.parentId should ===(1)
        node.childrenId should ===(List(5))
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "set string" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": "abc"
        |}""".stripMargin
    parser.input(json)
    parser.controller.setValue(3, "def")
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(5) match {
      case Some(node: StringNode) =>
        parser.code should ===(json.replace("abc", "def"))
        node.value should ===("\"" + "def" + "\"")
        node.code should ===("\"" + "def" + "\"")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "set number" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": 123
        |}""".stripMargin
    parser.input(json)
    parser.controller.setValue(3, 555)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(5) match {
      case Some(node: NumberNode) =>
        parser.code should ===(json.replace("123", "555"))
        node.value should ===(555d)
        node.code should ===("555")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "set boolean" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": true
        |}""".stripMargin
    parser.input(json)
    parser.controller.setValue(3, value = false)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(5) match {
      case Some(node: BooleanNode) =>
        parser.code should ===(json.replace("true", "false"))
        node.value should ===(false)
        node.code should ===("false")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "insert object node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.insertNode(ObjectNode.newValue(id = 3, parentId = 2))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(5) match {
      case Some(node: ObjectNode) =>
        parser.code should ===(json.replace("null", "{}"))
        node.code should ===("{}")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "insert number node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.insertNode(NumberNode.newValue(id = 3, value = 123, parentId = 2))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)

    ast.get(5) match {
      case Some(node: NumberNode) =>
        parser.code should ===(json.replace("null", "123"))
        node.code should ===("123")
        node.value should ===(123)
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }


}