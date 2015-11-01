package org.ababup1192.hybrid

object Main {

  def main(args: Array[String]): Unit = {

    // パース対象のJson
    val json =
      """{"foo": {"bar": [{"tes": null}, 1, "hoge"]}, "fizz": true}"""
    val parser = JsonParser()
    parser.input(json)

    println(parser.code)
    /*
    parser.controller.setKey(2, "foo2")
    parser.controller.setKey(4, "bar2")
    parser.controller.setKey(11, "buzz")
    parser.controller.setValue(16, value = false)
    parser.controller.setValue(9, 12)
    */

    /*
    parser.controller.delete(10)
    parser.controller.delete(6)
    parser.controller.delete(19)
    parser.controller.delete(17)
    parser.controller.delete(14)
    */

    /*
    parser.controller.swapArray(6, 9)
    parser.controller.swapArray(13, 10)
    */

    parser.controller.addEntry(1, "buzz")

    parser.controller.insertNode(NumberNode(14, "14", 14, -1, List.empty))


    parser.controller.addArrayElement(ArrayNode(14, "[]", 5, List.empty))

    println(parser.ast.mkString("\n"))

    println(parser.code)
  }

}