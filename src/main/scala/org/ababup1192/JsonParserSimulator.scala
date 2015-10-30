package org.ababup1192

import name.lakhin.eliah.projects.papacarlo.examples.Json
import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.syntax.Node

// papacarlo.syntax.Node型を少し扱いやすくした型
case class MyNode(id: Int, kind: String, value: List[String], parentId: Int, childrenId: List[Int], fragment: Option[MyFragment])

case class MyFragment(id: Int, from: WordPosition, to: WordPosition)

case class WordPosition(line: Int, ch: Int)

object JsonParserSimulator {

  private[this] var json = new String
  // 追加したノードのID 初期値rootノード
  private[this] var addedNodes = List(1)
  // private[this] var removedNodes = List.empty[Int]

  private[this] val jsonLexer = Json.lexer
  private[this] val jsonSyntax = Json.syntax(jsonLexer)

  def main(args: Array[String]): Unit = {
    // ノードが作られるごとにIDをListに追加。
    jsonSyntax.onNodeCreate.bind { node =>
      addedNodes = node.getId :: addedNodes
    }

    // パース対象のJson
    json = """{"foo": {"bar": [1, 2, [3, 4], 4]}, "fizz": true}"""
    jsonLexer.input(json)

    println(json)

    ast.foreach { node =>
      val value = node.fragment.map(f => json.substring(f.from.ch - 1, f.to.ch - 1)).getOrElse("")
      println(f"[${node.id}%2d, ${node.kind}%8s]: $value")
    }

  }

  def inputAll(text: String) = {
    json = text
    jsonLexer.input(text)
  }

  def setKey(id: Int, key: String): Unit = {
    jsonSyntax.getNode(id) foreach { node =>
      if (node.getKind == "entry") {
        node.getValues.flatMap(_._2).foreach { value =>
          getNodeFragment(id) foreach { fragment =>
            val target = json.substring(fragment.from.ch - 1, fragment.to.ch)
            val replaced = target.replace(value, s""""$key"""")

            val builder = StringBuilder.newBuilder
            builder.append(json.substring(0, fragment.from.ch - 1))
            builder.append(replaced)
            builder.append(json.substring(fragment.to.ch, json.length))

            val newJson = builder.toString()

            inputAll(newJson)
          }
        }
      }
    }
  }

  def ast: List[MyNode] = {
    val ast = addedNodes.reverse.foldLeft(List.empty[MyNode]) { (list, id) =>
      jsonSyntax.getNode(id) match {
        case Some(node) => exportMyNode(node) :: list
        case None => list
      }
    }.reverse
    ast
  }

  private def exportMyNode(node: Node): MyNode = {
    val parentId = node.getParent.map(_.getId).getOrElse(-1)

    val childrenId = node.getBranches.flatMap(_._2).map(_.getId).toList
    val values = node.getValues.flatMap(_._2).toList
    val fragment = getNodeFragment(node.getId)

    MyNode(node.getId, node.getKind, values, parentId, childrenId, fragment)
  }

  def getNodeFragment(id: Int): Option[MyFragment] = {
    jsonSyntax.getNode(id).map { node =>
      Some(MyFragment(id, tokenCursor(node.getBegin), tokenCursor(node.getEnd, after = true)))
    }.getOrElse(None)
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false): WordPosition = {
    token.collection.cursor(token.index + (if (after) 1 else 0)) match {
      case (line, ch) => WordPosition(line, ch)
    }
  }

}