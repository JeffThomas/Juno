package com.twilightfair.juno.language.asoft.parsers

import com.twilightfair.juno.language.asoft.elements.{ForElement, CommaConcatElement, IdentifierElement}
import com.twilightfair.juno.language.asoft.elements.configs.NextConfig
import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.parse.{Parser, _}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/12/13.
 */
case class NextParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence) {

  def getFirstIdent(
     element: Element,
     lexx: Lexx)
     : Either[ParserError, IdentifierElement] = {

    element match {
      case i: IdentifierElement => Right(i)
      case c: CommaConcatElement => {
        c.left match {
          case id: IdentifierElement => Right(id)
          case co: CommaConcatElement => getFirstIdent(co, lexx)
          case _ => Left(ParserError(s"Unkown element for NEXT ${c.left}", lexx))
        }
      }
    }
  }

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.parseNext(precedence) fold (
      e => Left(ParserError(s"No right hand element for prefix ${parser.lexx.currentToken.get}", parser.lexx, None)),
      rightParser => {
        val iters = rightParser.currentElementOpt.get
        getFirstIdent(iters, parser.lexx) fold (
            e => Left(e),
            firstIter => {
              val innermostFor: ForElement = parser.data("For").asInstanceOf[HashMap[String, ForElement]](firstIter.value)
              element(new NextConfig(
                parser.lexx,
                parser.data,
                rightParser.currentElementOpt.get,
                innermostFor)
              ) fold (
                e => Left(e),
                el => Right(ParseState(rightParser, el))
                )
            }
          )
      }
    )
  }
}
