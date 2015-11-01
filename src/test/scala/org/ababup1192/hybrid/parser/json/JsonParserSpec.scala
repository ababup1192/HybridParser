package org.ababup1192.hybrid.parser.json

import org.ababup1192.hybrid.parser._
import org.scalatest._

class JsonParserSpec extends FlatSpec with Matchers {

  "JsonParser" should "parse an empty object{}" in {
    val parser = JsonParser()
    parser.input("{}")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast

    ast.size should ===(1)
    ast.get(ast.size) === Some(ObjectNode)
  }

  "JsonParser" should "parse an entry with null" in {
    val parser = JsonParser()
    parser.input( """{"foo": null}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(3)

    ast.get(1) match {
      case Some(ObjectNode(id, code, parentId, childrenId)) =>
        childrenId should ===(List(2))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(2) match {
      case Some(EntryNode(id, code, entryKey, parentId, childrenId)) =>
        entryKey should ===("\"" + "foo" + "\"")
        parentId should ===(1)
        childrenId should ===(List(3))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(3) match {
      case Some(_: NullNode) => true should ===(true)
      case _ => throw new Exception("Type mismatch")
    }
  }

  "JsonParser" should "parse an object with entries" in {
    val parser = JsonParser()
    parser.input( """{"foo": null, "bar": null}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(5)

    ast.get(1) match {
      case Some(ObjectNode(id, code, parentId, childrenId)) =>
        childrenId should ===(List(2, 4))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(2) match {
      case Some(EntryNode(id, code, entryKey, parentId, childrenId)) =>
        entryKey should ===("\"" + "foo" + "\"")
        parentId should ===(1)
        childrenId should ===(List(3))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(3) match {
      case Some(NullNode(id, code, parentId, childrenId)) =>
        parentId should ===(2)
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(4) match {
      case Some(EntryNode(id, code, entryKey, parentId, childrenId)) =>
        entryKey should ===("\"" + "bar" + "\"")
        parentId should ===(1)
        childrenId should ===(List(5))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(5) match {
      case Some(NullNode(id, code, parentId, childrenId)) =>
        parentId should ===(4)
      case _ => throw new Exception("Type mismatch")
    }


  }

  "JsonParser" should "parse an entry with empty object{}" in {
    val parser = JsonParser()
    parser.input( """{"foo": {}}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(3)

    ast.get(ast.size) match {
      case Some(ObjectNode(id, code, parentId, childrenId)) =>
        code should ===("{}")
        parentId should ===(2)
        childrenId should ===(List.empty)
      case _ => throw new Exception("Type mismatch")
    }
  }

  "JsonParser" should "parse an entry with empty array[]" in {
    val parser = JsonParser()
    parser.input( """{"foo": []}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(3)

    ast.get(ast.size) match {
      case Some(ArrayNode(id, code, parentId, childrenId)) =>
        code should ===("[]")
        parentId should ===(2)
        childrenId should ===(List.empty)
      case _ => throw new Exception("Type mismatch")
    }
  }

  "JsonParser" should "parse an entry with number" in {
    val parser = JsonParser()
    parser.input( """{"foo": 123}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast

    ast.size should ===(3)
    ast.get(ast.size) match {
      case Some(NumberNode(id, code, num, parentId, childrenId)) =>
        code should ===("123")
        num should ===(123d)
      case _ => throw new Exception("Type mismatch.")
    }
  }

  "JsonParser" should "parse an entry with string" in {
    val parser = JsonParser()
    parser.input( """{"foo": "bar"}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast

    ast.size should ===(3)

    ast.get(ast.size) match {
      case Some(StringNode(id, code, text, parentId, childrenId)) =>
        code should ===("\"" + "bar" + "\"")
        text should ===("\"" + "bar" + "\"")
      case _ => throw new Exception("Type mismatch.")
    }
  }

  "JsonParser" should "parse entries with boolean values" in {
    val parser = JsonParser()
    parser.input( """{"foo": true, "bar": false}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast

    ast.size should ===(5)

    ast.get(3) match {
      case Some(BooleanNode(id, code, logicalValue, parentId, childrenId)) =>
        code should ===("true")
        logicalValue should ===(true)
      case _ => throw new Exception("Type mismatch.")
    }

    ast.get(5) match {
      case Some(BooleanNode(id, code, logicalValue, parentId, childrenId)) =>
        code should ===("false")
        logicalValue should ===(false)
      case _ => throw new Exception("Type mismatch.")
    }
  }

  "JsonParser" should "parse an entry with array" in {
    val parser = JsonParser()
    parser.input( """{"foo": [null, [], {}, 1, "string", true]}""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(9)

    ast.get(3) match {
      case Some(ArrayNode(id, code, parentId, childrenId)) =>
        code should ===( """[null, [], {}, 1, "string", true]""")
        parentId should ===(2)
        childrenId should ===(List(4, 5, 6, 7, 8, 9))
      case _ => throw new Exception("Type mismatch")
    }
  }

  "JsonParser" should "parse complex json" in {
    val parser = JsonParser()
    parser.input(
      """{
                "humans": [
                  {
                    "name"     : "John",
                    "age"      : 32,
                    "children" : ["John2", "John3"]
                  },
                  {
                    "name"     : "Mike",
                    "age"      : 35,
                    "children" : ["John2", "John3"]
                  },
                  {
                    "name"     : "Mary",
                    "age"      : 20,
                    "children" : []
                  }
                ]
              }""")
    parser.syntax.getErrors should ===(List.empty)
    val ast = parser.ast
    ast.size should ===(28)

    ast.get(1) match {
      case Some(ObjectNode(id, code, parentId, childrenId)) =>
        childrenId should ===(List(2))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(2) match {
      case Some(EntryNode(id, code, entryKey, parentId, childrenId)) =>
        entryKey should ===("\"" + "humans" + "\"")
        parentId should ===(1)
        childrenId should ===(List(3))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(3) match {
      case Some(ArrayNode(id, code, parentId, childrenId)) =>
        parentId should ===(2)
        childrenId should ===(List(4, 13, 22))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(4) match {
      case Some(ObjectNode(id, code, parentId, childrenId)) =>
        parentId should ===(3)
        childrenId should ===(List(5, 7, 9))
      case _ => throw new Exception("Type mismatch")
    }

    ast.get(10) match {
      case Some(ArrayNode(id, code, parentId, childrenId)) =>
        parentId should ===(9)
        code should ===("""["John2", "John3"]""")
        childrenId should ===(List(11, 12))
      case _ => throw new Exception("Type mismatch")
    }
  }
}
