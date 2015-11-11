package org.ababup1192.util

import upickle.Js._

object JsonUtil {

  object JsonBoolHelper {

    implicit class BoolTransformer(value: Boolean) {
      def toBool = if (value) True else False
    }

  }

}
