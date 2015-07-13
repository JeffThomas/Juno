package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.parse.elements.configs.ElementConfig

/**
 * Created by jthomas on 12/16/13.
 */
case class TestParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    element(new ElementConfig(parser.lexx, None, parser.data)) fold (
      e => Left(e),
      element => Right(ParseState(parser, element))
    )
  }
}
