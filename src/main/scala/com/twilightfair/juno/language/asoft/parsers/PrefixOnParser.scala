package com.twilightfair.juno.language.asoft.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.parse.elements.configs.PrefixConfig

/**
 * Created by jthomas on 12/12/13.
 */
case class PrefixOnParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence) {

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.data("On") = true
    parser.parseNext(precedence) fold (
      e => Left(ParserError(s"No right hand element for prefix ${parser.lexx.currentToken.get}", parser.lexx, None)),
      rightParser => {
        element(new PrefixConfig(
          parser.lexx,
          parser.data,
          rightParser.currentElementOpt.get)
        ) fold (
          e => Left(e),
          el => Right(ParseState(rightParser, el))
        )
      }
    )
  }
}
