package com.twilightfair.juno.parse

import com.twilightfair.juno.lexx.Lexx

/**
 * Created by jthomas on 5/1/14.
 */
class ParserError(val state: String, lexx: Lexx, val data: Option[Any] = None) {
}

object ParserError {
  def apply(state: String, lexx: Lexx, data: Option[Any] = None, hide: Boolean = false) = {
    new ParserError(state, lexx, data)
  }
}
