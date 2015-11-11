package org.ababup1192

import org.ababup1192.parser.drawing.JsonVisitor
import org.ababup1192.parser.json.JsonParser

object ParserTest {
  def main(args: Array[String]) {
    val parser = JsonParser()
    parser.input("""{"foo": [1,2,3], "bar": null""")

    parser.drawingAst.foreach(ast =>
      JsonVisitor.parse(upickle.json.read(upickle.json.write(ast.toJson))).foreach(println))

  }
}
