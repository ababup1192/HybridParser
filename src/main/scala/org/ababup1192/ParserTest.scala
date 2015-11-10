package org.ababup1192

import org.ababup1192.parser.json.JsonParser

object ParserTest {
  def main(args: Array[String]) {
    val parser = JsonParser()
    parser.input( """{"hoge": ["str", 2, true, false, {"foo": true, "bar": false}], "uge": 123}""")

    parser.drawingAst.foreach(println)
  }
}
