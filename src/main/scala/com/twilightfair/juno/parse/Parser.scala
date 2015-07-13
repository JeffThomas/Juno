package com.twilightfair.juno.parse

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.lexx.{LexxConfig, Lexx}
import com.twilightfair.juno.parse.Parser.ParseLex
import com.twilightfair.juno.parse.elements.NopElement
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import com.twilightfair.juno.language.test.Tester
import com.twilightfair.juno.lexx.matchers.MatcherState
import scala.collection.mutable
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

/**
 * Created by jthomas on 12/12/13.
 */
case class Parser(
  lexx: Lexx,
  lang: Language,
  previousState: Option[Parser] = None,
  currentElementOpt: Option[Element] = None,
  data: mutable.HashMap[String, Any] = mutable.HashMap.empty[String, Any]
) {

  def previous(): Option[Element] = {
    previousState.flatMap{ _.currentElementOpt }
  }

  private def getDefinedTokens(parsers: List[(String, Option[String], TokenParser)], tokenType: String, token: String)
     : List[TokenParser] = {
    parsers.filter { parserNode =>
      val res = parserNode._1 == tokenType && (parserNode._2 == Some(token) || parserNode._2.isEmpty)
      //println(s"----> ${tokenType} : ${token} == ${parserNode.toString} : $res")
      res
    }.map(_._3)
  }

  private def getNextTokenParser(
      lexx: Lexx,
      tokens: List[(String, Option[String], TokenParser)])
      : Either[ParserError, List[ParseLex]] = {

    Parser.nextLexxWhitespaceHandler(lexx) fold (
      e => {
        Parser.logger.debug(s"did not find token for '${lexx.text(lexx.index)}'")
        Left(ParserError(s"Syntax Error at character index ${lexx.index}: '${lexx.text(lexx.index)}'", lexx, Some(e)))
      },
      lexxOpt => { lexxOpt.map { lexx =>
        val token = lexx.head.get.result
        if (token.success) {
          val parsers = getDefinedTokens(tokens, token.tokenType, token.value).map {
            ParseLex(_, lexx)
          }
          if (parsers.isEmpty){
            Left(ParserError(s"No parser found for ${token.value}", lexx, None))
          } else {
            Right(parsers)
          }
        } else {
          Left(ParserError("error", lexx, Some(token.value)))
        }}.getOrElse(Left(ParserError("Done: No more tokens", lexx, None)))
      }
    )
  }

  private def parseNextInfix(state: ParseState, tokens: List[(String, Option[String], TokenParser)], precedence: Int)
      : Either[ParserError, ParseState] = {
    Parser.logger.debug("parseNextInfix")
    getNextTokenParser(state.parser.lexx, tokens) match {
      case Left(e) =>
        Parser.logger.debug(s"No infix token parsers returned from getNextTokenParser")
        Right(state)
      case Right(nextTokenParsers) =>
        Parser.logger.debug(s"${nextTokenParsers.length} infix parsers found for token: ${nextTokenParsers(0).lexx.currentToken.toString}")
        nextTokenParsers.foldLeft[Either[ParserError, ParseState]](Right(state)) { (result, nextTokenParser) =>
          Parser.logger.debug(s"parsing: ${nextTokenParser.lexx.currentToken.toString} with parser ${nextTokenParser.toString}")
          if (precedence < nextTokenParser.parser.precedence) {
            val data = state.parser.lang.preElement(nextTokenParser, this.data)
            data._1.parse(Parser(nextTokenParser.lexx, lang, Some(state.parser), state.element, data._2)) match {
              case Left(e) =>
                Parser.logger.debug(s"error returned from parse: ${e.toString}")
                Left(e)
              case Right(parseState) => {
                if (parseState.element.isDefined && parseState.element.get.isInstanceOf[NopElement]) {
                  result
                } else {
                  Parser.logger.debug(s"infix parse success generated: ${parseState.element.toString}")
                  // HA!
                  return parseNextInfix(parseState, tokens, precedence) fold(
                    e => result,
                    newState => Right(newState)
                  )
                }
              }
            }
          } else {
            Parser.logger.debug(s"precedence skip, precedence $precedence : parser precedence ${nextTokenParser.parser.precedence}")
            result
          }
        }
    }
  }

  def parseNext(matchers: (Int) => List[MatcherState], precedence: Int): Either[ParserError, Parser] = {
    class TempConfig(matcherList: (Int) => List[MatcherState]) extends LexxConfig {
      override def matchers(location:Int): List[MatcherState] = { matcherList(location) }
    }
    val tempParser: Parser = this.copy(lexx = this.lexx.copy(config = new TempConfig(matchers)))
    tempParser.parseNext(precedence) fold (
        e => Left(e),
        p => Right(p.copy(lexx = p.lexx.copy(config = this.lexx.config)))
      )
  }

  def parseNext(precedence: Int): Either[ParserError, Parser] = {
    Parser.logger.debug("parseNext")
    getNextTokenParser(lexx, lang.prefixTokens) fold (
      error => {
        Parser.logger.debug(s"No prefix token parsers returned from getNextTokenParser")
        Left(error)
      },
      parsers => {
        Parser.logger.debug(s"${parsers.length} prefix parsers found for token: ${parsers(0).lexx.currentToken.toString}")
        parsers.foldLeft[Option[Either[ParserError, Parser]]](None) { (r, parser) =>
          val data: (TokenParser, mutable.HashMap[String, Any]) = lang.preElement(parser, this.data)
          Parser.logger.debug(s"parsing: ${parser.lexx.currentToken.toString} with parser ${data._1.toString}")
          data._1.parse(Parser(parser.lexx, lang, Some(this), None, data._2)) fold(
            parseError => {
              Parser.logger.debug(s"parse failed: ${parseError.toString}")
              Some(Left(parseError))
            },
            parseState => {
              parseState.element match {
                case Some(_) =>
                  Parser.logger.debug(s"prefix parse success generated: ${parseState.element.get.toString}")
                  parseNextInfix(parseState, lang.infixTokens, precedence) match {
                    case Right(nextParseState) => {
                      Some(Right(Parser(
                        nextParseState.parser.lexx,
                        lang,
                        Some(nextParseState.parser),
                        nextParseState.element,
                        nextParseState.parser.data
                      )))
                    }
                    case Left(nextParseError) => {
                      Parser.logger.debug(s"No parser/token found: ${nextParseError.state}")
                      Some(Left(nextParseError))
                    }
                  }
                case None =>
                  Parser.logger.debug("No element returned from parse")
                  Some(Right(Parser(parser.lexx, lang, Some(this), None, data._2)))
              }
            }
          )
        }.get
      }
    )
  }
}

object Parser {

  /**
   * This is creating one static logger. This is going to need to change
   * in a fully dynamic environment.
   */
  val logger = Logger(LoggerFactory.getLogger("Parser"))

  def reverseElements(list: List[Element]): List[Element] = {
    var result: List[Element] = Nil
    var these = list
    while (!these.isEmpty) {
      these.head.next = result.headOption
      result = these.head :: result
      these = these.tail
    }
    result
  }

  case class ParseLex(parser: TokenParser, lexx: Lexx)

  @tailrec final def nextLexxWhitespaceHandler(lexx: Lexx): Either[List[MatcherState],Option[Lexx]] = {
    lexx.nextOne match {
      case Left(e) => Left(e)
      case Right(lexOpt) => lexOpt match {
        case None => Right(None)
        case Some(lexx) if lexx.currentToken.isDefined
          && lexx.currentToken.get.tokenType == "Whitespace" =>
          nextLexxWhitespaceHandler(lexOpt.get)
        case Some(lexx) => Right(Some(lexx))
      }
    }
  }

  @tailrec final def parse(parser: Parser, elements: List[Element], precedence: Int): Out = {
    val index = if (parser.lexx.index < 0) 0 else parser.lexx.index
    val sc = parser.lexx.text.mkString("")
    val end = if (index + 10 < sc.length) index + 10 else sc.length
    logger.debug(s"parse: ${sc.substring(index, end)}")
    parser.parseNext(precedence) match {
      case Left(e) =>
        logger.debug(s"Error: ${e.state}")
        Out(reverseElements(elements), Some(e), parser)
      case Right(nextParser) =>
        nextParser.currentElementOpt match {
          case None => {
            logger.debug(s"No element returned from parseNext")
            Out(elements, None, nextParser)
          }
          case Some(element) =>
            parse(nextParser, element :: elements, precedence)
        }
      }
  }
}

object Main {
  def main(args: Array[String]):Unit = {
    val test = "5 +\n7 * 4\n +\n 1"
    val lexer: Lexx = Lexx(Tester.lexxConfig, test.toStream)

    val results = Parser(lexer, Tester).parseNext(0)
    //val results = Parser.parse(Parser(Tester, lexer), Nil, 0)
    results.right map { r =>
      r.currentElementOpt map { element =>
        println(element.graph("")._2)
      }
    }
  }
}
