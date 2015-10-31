package org.ababup1192.hybrid

object Main {

  def main(args: Array[String]): Unit = {

    // パース対象のJson
    val json =
      """{"foo": {"bar": [{"tes": null}, 1, null]}, "fizz": true}"""
    val parser = JsonParser()
    parser.input(json)
    println(json)
    println(parser.ast.mkString("\n"))
  }

}