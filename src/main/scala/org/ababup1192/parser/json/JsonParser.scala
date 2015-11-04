package org.ababup1192.parser.json

import name.lakhin.eliah.projects.papacarlo.examples.Json
import org.ababup1192.parser._
import spray.json._

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

  /**
    * Create an json AST for convenience.
    * @return AST ID -> js.Any
    */
  def jsonAst: JsValue = {
    import JsonParser.ASTJsonProtocol._
    ast.toJson
  }

}

object JsonParser {

  object ASTJsonProtocol extends DefaultJsonProtocol {

    implicit object MapNodeJsonFormat extends JsonFormat[Map[Int, Node]] {
      def write(m: Map[Int, Node]) = {
        JsObject(m.map {
          case (key, value: ObjectNode) => (key.toString, value.toJson)
          case (key, value: EntryNode) => (key.toString, value.toJson)
          case (key, value: ArrayNode) => (key.toString, value.toJson)
          case (key, value: StringNode) => (key.toString, value.toJson)
          case (key, value: NumberNode) => (key.toString, value.toJson)
          case (key, value: BooleanNode) => (key.toString, value.toJson)
          case (key, value: NullNode) => (key.toString, value.toJson)
        })
      }

      def read(value: JsValue) = {
        (for {
          (key, v) <- value.asJsObject.fields
          kind <- v.asJsObject.fields.get("kind")
        } yield {
          kind match {
            case JsString("object") => (key.toInt, ObjectNode.fromJson(v))
            case JsString("entry") => (key.toInt, EntryNode.fromJson(v))
            case JsString("array") => (key.toInt, ArrayNode.fromJson(v))
            case JsString("string") => (key.toInt, StringNode.fromJson(v))
            case JsString("number") => (key.toInt, NumberNode.fromJson(v))
            case JsString("boolean") => (key.toInt, BooleanNode.fromJson(v))
            case JsString("null") => (key.toInt, NullNode.fromJson(v))
          }
        }).flatMap {
          case (key, Some(node: Node)) => Map(key -> node)
        }
      }
    }

  }

}

