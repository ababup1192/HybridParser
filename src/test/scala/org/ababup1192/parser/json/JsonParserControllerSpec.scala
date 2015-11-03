package org.ababup1192.parser.json

import org.ababup1192.parser._
import org.scalatest._

class JsonParserControllerSpec extends FlatSpec with Matchers {

  "JsonParserController" should "set entry node key" in {
    val parser = JsonParser()
    parser.input( """{"foo": 123}""")
    parser.controller.setKey(2, "bar")
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)
    parser.code should ===( """{"bar": 123}""")

    ast.get(4).foreach {
      case node: EntryNode =>
        node.key should ===("bar")
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
    parser.code should ===(json.replace("abc", "def"))

    ast.get(5) match {
      case Some(node: StringNode) =>
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
    parser.code should ===(json.replace("123", "555"))

    ast.get(5) match {
      case Some(node: NumberNode) =>
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
    parser.code should ===(json.replace("true", "false"))

    ast.get(5) match {
      case Some(node: BooleanNode) =>
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
    parser.code should ===(json.replace("null", "{}"))

    ast.get(5) match {
      case Some(node: ObjectNode) =>
        node.code should ===("{}")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "insert array node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.insertNode(ArrayNode.newValue(id = 3, parentId = 2))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)
    parser.code should ===(json.replace("null", "[]"))

    ast.get(5) match {
      case Some(node: ArrayNode) =>
        node.code should ===("[]")
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "insert string node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.insertNode(StringNode.newValue(id = 3, value = "abc", parentId = 2))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)
    parser.code should ===(json.replace("null", "\"abc\""))

    ast.get(5) match {
      case Some(node: StringNode) =>
        node.code should ===("\"abc\"")
        node.value should ===("\"abc\"")
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
    parser.code should ===(json.replace("null", "123"))

    ast.get(5) match {
      case Some(node: NumberNode) =>
        node.code should ===("123")
        node.value should ===(123)
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "insert boolean node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.insertNode(BooleanNode.newValue(id = 3, value = true, parentId = 2))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)
    parser.code should ===(json.replace("null", "true"))

    ast.get(5) match {
      case Some(node: BooleanNode) =>
        node.code should ===("true")
        node.value should ===(true)
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "add entry node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": null
        |}""".stripMargin
    parser.input(json)
    parser.controller.addEntry(1, "bar")
    parser.controller.addEntry(1, "fizz")
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(7)
    parser.code should ===( """{"foo": null, "bar": null, "fizz": null}""")

    ast.get(6) match {
      case Some(node: EntryNode) =>
        node.key should ===("bar")
        node.parentId should ===(1)
        node.childrenId should ===(List(7))
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(7) match {
      case Some(node: NullNode) =>
        node.code should ===("null")
        node.parentId should ===(6)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(8) match {
      case Some(node: EntryNode) =>
        node.key should ===("fizz")
        node.parentId should ===(1)
        node.childrenId should ===(List(9))
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(9) match {
      case Some(node: NullNode) =>
        node.code should ===("null")
        node.parentId should ===(8)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "add array element node" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": []
        |}""".stripMargin
    parser.input(json)
    parser.controller.addArrayElement(NumberNode.newValue(value = 1, parentId = 3))
    parser.controller.addArrayElement(NumberNode.newValue(value = 2, parentId = 3))
    parser.controller.addArrayElement(NumberNode.newValue(value = 3, parentId = 3))
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(6)
    parser.code should ===(json.replace("[]", "[1, 2, 3]"))

    ast.get(3) match {
      case Some(node: ArrayNode) =>
        node.parentId should ===(2)
        node.childrenId should ===(List(4, 5, 6))
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(4) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(1)
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(5) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(2)
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(6) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(3)
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "swap array elements" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": [1, 2, 3]
        |}""".stripMargin
    parser.input(json)
    parser.controller.swapArray(4, 5)
    parser.controller.swapArray(7, 6)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(6)
    parser.code should ===(json.replace("[1, 2, 3]", "[3, 1, 2]"))

    ast.get(3) match {
      case Some(node: ArrayNode) =>
        node.parentId should ===(2)
        node.childrenId should ===(List(9, 10, 11))
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(9) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(3)
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(10) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(1)
      case _ =>
        throw new Exception("Type mismatch")
    }

    ast.get(11) match {
      case Some(node: NumberNode) =>
        node.parentId should ===(3)
        node.value should ===(2)
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "delete entry value" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": {}
        |}""".stripMargin
    parser.input(json)
    parser.controller.delete(3)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(3)
    parser.code should ===(json.replace("{}", "null"))

    ast.get(5) match {
      case Some(node: NullNode) =>
        node.parentId should ===(4)
        node.childrenId should ===(List())
      case _ =>
        throw new Exception("Type mismatch")
    }
  }

  "JsonParserController" should "delete array elements" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": [1, 2, 3, 4]
        |}""".stripMargin
    parser.input(json)
    parser.controller.delete(5)
    parser.controller.delete(7)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(5)
    parser.code should ===(json.replace("[1, 2, 3, 4]", "[1, 3]"))
  }

  "JsonParserController" should "delete entry" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": 1,
        |  "bar": 2,
        |  "fizz": 3,
        |  "buzz": 4
        |}""".stripMargin
    parser.input(json)
    parser.controller.delete(4)
    parser.controller.delete(14)
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(5)
    parser.code should ===("""{"foo": 1, "fizz": 3}""")
  }

   "JsonParserController" should "delete array elements" in {
    val parser = JsonParser()
    val json =
      """{
        |  "foo": [1, 2, 3, 4]
        |}""".stripMargin
    parser.input(json)
    parser.controller.delete(5)
    parser.controller.delete(7)
    parser.syntax.getErrors should ===(List.empty)

    val ast = parser.ast
    ast.size should ===(5)
    parser.code should ===(json.replace("[1, 2, 3, 4]", "[1, 3]"))
  }
}