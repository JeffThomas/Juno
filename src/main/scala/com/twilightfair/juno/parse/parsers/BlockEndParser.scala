package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, BlockConfig}
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.Constants

/**
 * Created by jthomas on 2/5/14.
 */
case class BlockEndParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    // just kill this stream of parsing
    Right(ParseState(parser, None))
  }
}
