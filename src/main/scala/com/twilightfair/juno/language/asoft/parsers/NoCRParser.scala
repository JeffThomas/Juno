package com.twilightfair.juno.language.asoft.parsers

import com.twilightfair.juno.language.asoft.elements.PrintElement
import com.twilightfair.juno.parse.{Parser, _}
import com.twilightfair.juno.parse.elements.configs.{PostfixConfig, ElementConfig}

/**
 * Created by jthomas on 12/12/13.
 */
case class NoCRParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.currentElementOpt.map { currentElement =>
      parser.data("PrintCr") = false
      Right(ParseState(parser, Some(currentElement)))
    }.getOrElse(Left(ParserError(s"No left hand element for infix ${parser.lexx.currentToken.get}", parser.lexx, None)))
  }
}
