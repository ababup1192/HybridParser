package org.ababup1192.hybrid.util

object StringUtil {

  def splitString(str: String, from: Int, to: Int): (String, String, String) = {
    (str.substring(0, from), str.substring(from, to), str.substring(to, str.length))
  }

}
