package org.ababup1192

import org.ababup1192.parser._
import org.ababup1192.parser.json.JsonParser

/**
  * Created by abab on 11/4/15.
  */
object ParserTest {
  def main(args: Array[String]) {
    val parser = JsonParser()
    parser.input( """{"hoge": ["str", 2, true, false, null]}""")

    import JsonParser.ASTJsonProtocol._
    val map = parser.jsonAst.convertTo[Map[Int, Node]]

    println(map)
  }
}
