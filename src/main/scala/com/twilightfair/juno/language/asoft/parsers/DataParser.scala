package com.twilightfair.juno.language.asoft.parsers

import java.lang.Process

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.language.asoft.elements.CommaConcatElement
import com.twilightfair.juno.language.asoft.matchers.UnquotedStringMatcher
import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.elements.StringElement
import com.twilightfair.juno.parse.{Parser, _}
import com.twilightfair.juno.parse.elements.configs.PrefixConfig

import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * Created by jthomas on 12/12/13.
 */
case class DataParser(override val element: ElementFactory, override val precedence: Int)
    extends TokenParser(element, precedence) {

  def matchers(location:Int): List[MatcherState] = List(
    StringMatcher.init(location),
    UnquotedStringMatcher.init(location),
    SymbolMatcher.init(location, List(",")),
    WhitespaceMatcher.init(location, false)
  )

  def elementsToList(el: Element): List[String] = {
    el match {
      case s: StringElement => {
        List(s.value)
      }
      case c: CommaConcatElement => elementsToList(c.left) ++ elementsToList(c.right)
    }
  }

  def parse(parser: Parser): Either[ParserError, ParseState] = {
    parser.parseNext(matchers, precedence) fold (
      e => Left(ParserError(s"No right hand element for prefix ${parser.lexx.currentToken.get}", parser.lexx, None)),
      rightParser => {
        val d = elementsToList(rightParser.currentElementOpt.get)
        parser.data("Data") = parser.data("Data").asInstanceOf[List[String]] ++ d
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
