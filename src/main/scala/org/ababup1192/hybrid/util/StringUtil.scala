package org.ababup1192.hybrid.util

object StringUtil {

  /**
   * Split three Strings
   * Ex. "abcdef" -> ("ab", "cd", "ef")
   * @param str target String
   * @param from start index of center String
   * @param to end index of center String
   * @return (prefix, center, suffix)
   */
  def splitString(str: String, from: Int, to: Int): (String, String, String) = {
    (str.substring(0, from), str.substring(from, to), str.substring(to, str.length))
  }

}
