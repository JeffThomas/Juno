package com.twilightfair.juno.parse

import com.twilightfair.juno.lexx.Lexx

/**
 * Created by jthomas on 12/12/13.
 */
abstract class TokenParser(val element: ElementFactory, val precedence: Int) {
  def parse(parser: Parser): Either[ParserError, ParseState]
}
