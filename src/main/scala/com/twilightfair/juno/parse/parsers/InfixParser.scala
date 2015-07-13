package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.parse.elements.configs.InfixConfig

/**
 * Created by jthomas on 12/12/13.
 */
case class InfixParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.currentElementOpt.map { currentElement =>
      parser.parseNext(precedence) fold (
        e => Left(ParserError(s"No right hand element for infix ${parser.lexx.currentToken.get}", parser.lexx, None)),
        rightParser => {
          element(new InfixConfig(
              parser.lexx,
              parser.data,
              currentElement,
              rightParser.currentElementOpt.get)
          ) fold (
            e => Left(e),
            el => Right(ParseState(rightParser, el))
          )
        }
      )
    }.getOrElse(Left(ParserError(s"No left hand element for infix ${parser.lexx.currentToken.get}", parser.lexx, None)))
  }
}
