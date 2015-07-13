package com.twilightfair.juno.language.asoft.parsers

import com.twilightfair.juno.parse.elements.BlockElement
import com.twilightfair.juno.parse.elements.configs.OptionPrefixConfig
import com.twilightfair.juno.parse.{Parser, _}
import com.twilightfair.juno.Constants._

/**
  * Created by jthomas on 12/12/13.
  */
case class PrefixIdentifierParser(override val element: ElementFactory, override val precedence: Int)
  extends TokenParser(element, precedence) {

     def parse(parser: Parser): Either[ParserError, ParseState] = {
       // get the next non space token
       val res = parser.lexx.nextOne(false) fold(
         e => None, // no next token
         r => {
           r.flatMap( l => l.currentToken.flatMap( t => t.value match {
             case "(" =>
               parser.parseNext(PRECEDENCE_CALL) fold (
                 e => None, // parsing the next token failed
                 rightParser => { // make an identifier with a sub block
                   if (rightParser.currentElementOpt.get.isInstanceOf[BlockElement])
                     Some(element(new OptionPrefixConfig(
                       parser.lexx,
                       rightParser.data,
                       Some(rightParser.currentElementOpt.get)
                     )) fold (
                       e => Left(e),
                       el => Right(ParseState(rightParser, el))
                     ))
                   else
                     None // Didn't make a BlockElement (why?)
                 }
               )
             case _ =>
               None // next token not "("
           }))
         }
       )
       val ret = res.getOrElse(
         makeIdentifierElement(parser)
       )
       ret
     }

  def makeIdentifierElement(parser: Parser): Either[ParserError, ParseState] = {
    element(
      new OptionPrefixConfig(parser.lexx, parser.data, None)
    ) fold(
      e => Left(e),
      el => Right(ParseState(parser, el))
    )
  }
}
