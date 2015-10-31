package org.ababup1192.hybrid

object Main {

  def main(args: Array[String]): Unit = {

    // パース対象のJson
    val json =
      """{"foo": {"bar": [{"tes": null}, 1, null]}, "fizz": true}"""
    val parser = JsonParser()
    parser.input(json)

    // println(parser.ast.mkString("\n"))

    println(parser.code)
    parser.controller.setKey(2, "foo2")
    parser.controller.setKey(4, "bar2")
    parser.controller.setKey(11, "buzz")
    println(parser.code)
  }

}