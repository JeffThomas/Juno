package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.elements.configs.PostfixConfig

/**
 * Created by jthomas on 7/2/14.
 */
case class PostfixParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence) {

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.currentElementOpt.map { currentElement =>
      element(new PostfixConfig(
        parser.lexx,
        parser.data,
        currentElement)
      ) fold (
        e => Left(e),
        el => Right(ParseState(parser, el))
      )
    }.getOrElse(Left(ParserError(s"No left hand element for infix ${parser.lexx.currentToken.get}", parser.lexx, None)))
  }
}
