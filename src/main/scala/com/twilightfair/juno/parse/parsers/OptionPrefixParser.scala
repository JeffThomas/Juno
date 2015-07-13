package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse.elements.configs.OptionPrefixConfig
import com.twilightfair.juno.parse.{Parser, _}

/**
 * Created by jthomas on 12/12/13.
 */
case class OptionPrefixParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence) {

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.parseNext(precedence) fold (
      e => // this is ok, it just means we aren't doing an array or function
        element(
          new OptionPrefixConfig(parser.lexx, parser.data, None)
        ) fold (
          e => Left(e),
          el => Right(ParseState(parser, el))
        ),
      rightParser => {
        element(new OptionPrefixConfig(
          parser.lexx,
          parser.data,
          Some(rightParser.currentElementOpt.get))
        ) fold (
          e => Left(e),
          el => Right(ParseState(rightParser, el))
        )
      }
    )
  }
}
