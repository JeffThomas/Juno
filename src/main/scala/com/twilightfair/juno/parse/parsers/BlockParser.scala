package com.twilightfair.juno.parse.parsers

import com.twilightfair.juno.parse._
import com.twilightfair.juno.parse.elements.configs.{BlockConfig, ElementConfig}
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.Constants

/**
 * Created by jthomas on 2/5/14.
 */
case class BlockParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence){

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    val out: Out = Parser.parse(parser, Nil, Constants.PRECEDENCE_NONE)
    element(new BlockConfig(parser.lexx, parser.data, out)) fold (
      e => Left(e),
      element =>
        Right(ParseState(out.parser, element))
    )
  }
}
