package org.ababup1192.hybrid.util

import org.ababup1192.hybrid.parser.WordLocation

object StringUtil {

  /**
   * Split three Strings
   * Ex. "abcdef" -> ("ab", "cd", "ef")
   * @param str target String
   * @param fromLocation start index of center String
   * @param toLocation end index of center String
   * @return (prefix, center, suffix)
   */
  def splitString(str: String, fromLocation: WordLocation, toLocation: WordLocation): (String, String, String) = {
    val strArr = str.split("\n")

    // Pre-line word length + current-line word length + newline length
    val from = (0 until fromLocation.line - 1).foldLeft(0) { (sum, index) =>
      sum + strArr(index).length
    } + (fromLocation.line - 1) + fromLocation.ch

    val to = (0 until toLocation.line - 1).foldLeft(0) { (sum, index) =>
      sum + strArr(index).length
    } + (toLocation.line - 1) + toLocation.ch

    (str.substring(0, from), str.substring(from, to), str.substring(to, str.length))
  }

}
