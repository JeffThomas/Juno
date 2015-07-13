package com.twilightfair.juno.language.asoft.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.parse.elements.NopElement
import com.twilightfair.juno.parse.elements.configs.InfixConfig
import com.twilightfair.juno.language.asoft.elements.configs.InfixConcatConfig
import com.twilightfair.juno.language.asoft.elements.{SeperatorElement, EOLElement}

/**
 * Created by jthomas on 12/12/13.
 */
  case class InfixConcatParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.currentElementOpt.map { currentElement =>
      parser.parseNext(precedence) fold (
        e => {
          Left(ParserError(s"No right hand element for infix ${parser.lexx.currentToken.get}", parser.lexx,
            Some(NopElement.emptyNopElement)))
        },
        rightParser => {
          if (rightParser.currentElementOpt.get.isInstanceOf[EOLElement]
              || rightParser.currentElementOpt.get.isInstanceOf[SeperatorElement]){
            Right(ParseState(parser, Some(NopElement.emptyNopElement)))
          } else {
            element(new InfixConcatConfig(
              parser.lexx,
              parser.data,
              currentElement,
              rightParser.currentElementOpt)
            ) fold(
              e => Left(e),
              el => Right(ParseState(rightParser, el))
            )
          }
        }
      )
    }.getOrElse(Left(ParserError(s"No left hand element for infix ${parser.lexx.currentToken.get}", parser.lexx, None)))
  }
}
