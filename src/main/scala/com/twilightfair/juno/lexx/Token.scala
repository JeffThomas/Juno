package com.twilightfair.juno.lexx

/**
 * Created by jthomas on 5/8/14.
 */
case class Token(
  tokenType: String,
  start: Int,
  length: Int,
  value: String,
  success: Boolean,
  data: Map[String, Any]
)

object Token {
  lazy val EOLToken = Token("", 0, 1, "\n", true, Map())
}
