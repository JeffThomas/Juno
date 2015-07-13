package com.twilightfair.juno.language.asoft.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.parse.elements.configs.ElementConfig

/**
 * Created by jthomas on 7/8/14.
 */
case class EOLParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.data("EOL") = true
    element(new ElementConfig(parser.lexx, None, parser.data)) fold (
      e => Left(e),
      element => Right(ParseState(parser, element))
    )
  }
}
