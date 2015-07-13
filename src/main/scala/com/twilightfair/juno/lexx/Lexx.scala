package com.twilightfair.juno.lexx

import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.{Parser, Out, TokenParser}
import com.twilightfair.juno.runtime
import com.typesafe.scalalogging.{Logger, LazyLogging}
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import com.twilightfair.juno.lexx.matchers.MatcherState.Matching
import com.twilightfair.juno.lexx.matchers.MatcherState.Success
import com.twilightfair.juno.lexx.matchers.MatcherState.Fail
import com.twilightfair.juno.language.test.BookReader
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.Future
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/26/13
 * Time: 6:06 PM
 * To change this template use File | Settings | File Templates.
 */


trait LexxConfig {
  def matchers(location:Int): List[MatcherState]
}

trait EmptyLexxConfig extends LexxConfig {
  override def matchers(location:Int): List[MatcherState] = Nil
}

case class Lexx(
  config: LexxConfig,
  text: Stream[Char],
  head: Option[MatcherState],
  tail: Option[Lexx],
  index: Int,
  lineCount: Int,
  lineStart: Int,
  length: Int,
  newLine: Boolean
) {

  private val logger = Logger(LoggerFactory.getLogger("Lexx"))

  val currentToken: Option[Token] = {
    head.map{_.result}
  }

  def getPosition(): (Int, Int) = {
    (lineCount, index - lineStart)
  }

  private def isValueEOL(value: String): Boolean = {
    val token = value
    token.contains("\r") || token.contains("\n") || token.contains("\r\n")
  }

  def nextOne(whitespace: Boolean): Either[List[MatcherState], Option[Lexx]] = {
    next(whitespace).fold(
      e => Left(e),
      l => Right(if (l.length > 0) Some(l(0)) else None)
    )
  }

  def next(whitespace: Boolean): Either[List[MatcherState], List[Lexx]] = {
    next.fold(
      e => Left(e),
      lexxs =>
        if (whitespace) {
          Right(lexxs)
        } else {
          if (lexxs.length > 0) {
            lexxs(0).currentToken.map(t => t.tokenType match {
              case "Whitespace" =>
                lexxs(0).next(false)
              case _ =>
                Right(lexxs)
            }).getOrElse(Right(Nil))
          } else {
            Right(Nil)
          }
        }
    )
  }

  def nextOne: Either[List[MatcherState], Option[Lexx]] = {
    next.fold(
      e => Left(e),
      l => Right(if (l.length > 0) Some(l(0)) else None)
    )
  }

  def next: Either[List[MatcherState], List[Lexx]] = {
    if (index < text.size){
      nextToken(config, text, index) fold (
        matcherStates => {
          logger.debug(s"SYNTAX ERROR: Failed to find match at ${text(index)}")
          Left(matcherStates)
        },
        lexxs => {
          Right(lexxs)
        }
      )
    } else {
      Right(Nil)
    }
  }

  private def nextToken(config: LexxConfig, text: Stream[Char], location: Int): Either[List[MatcherState], List[Lexx]] = {

    @tailrec def oneStep(curLocation: Int, matching: List[MatcherState], succeeded: List[Lexx],
                         failed: List[MatcherState]): Either[List[MatcherState], List[Lexx]] = {

      case class Results(
          success: List[Lexx],
          fail: List[MatcherState],
          cont: List[MatcherState]
      )

      val res = matching.map {
        _.step(text, curLocation)
      }.foldLeft(Results(succeeded, failed, Nil)){ (data, matcher) =>
        matcher match {
          case m: Matching => Results(data.success, data.fail, m :: data.cont)
          case s: Success => {
            val (nextLineCount, nextLineStart) =
              if (isValueEOL(s.result.value)) (lineCount + 1, index + s.result.length)
              else (lineCount, lineStart)
            val nextLine = isValueEOL(this.currentToken.getOrElse(Token.EOLToken).value)
            val lexx = Lexx(config, text, Some(s), Some(this), index + s.result.length, nextLineCount, nextLineStart, s.result.length, nextLine)
            Results(lexx :: data.success, data.fail, data.cont)
          }
          case f: Fail => Results(data.success, f :: data.fail, data.cont)
        }
      }

      if (res.cont.isEmpty){
        res.success.size match {
          case 1 =>
            Right(res.success)
          case s if s > 1 =>
            val sucSorted = res.success.sortBy(_.head.get.result.length).reverse
            Right(sucSorted)
          case 0 =>
            Left(res.fail)
        }
      } else {
        oneStep(curLocation+1, res.cont, res.success, res.fail)
      }
    }
    oneStep(location, config.matchers(location), Nil, Nil )
  }
}

object Lexx {

  def apply(config: LexxConfig, textIn: Stream[Char]) = {
    new Lexx(config, textIn, None, None, 0, 1, 0, 0, true)
  }

  val emptyLexx = Lexx(new Object with EmptyLexxConfig,Stream[Char](),None,None,0,0,0,0,false)
}

object Main {
  val sampleInput = "1234 ++ = identifier    \"test\" 5 14.15 _i_ j2 k for iftest_this \"more strings!\" 1.2+2=3.2"

  val sampleBook = Source.fromFile("c:/Users/jeff/dropbox/projects/juno.sc/AlicesAdventuresInWonderland.txt").mkString

  def main(args: Array[String]):Unit = {

    val lexer: Lexx = Lexx(BookReader.lexxConfig, sampleBook.toCharArray.toStream)

    println("Starting.")

    var whitespace: Int = 0
    var words: Int = 0
    var ints: Int = 0
    var floats: Int = 0
    var symbols: Int = 0

    @tailrec def consumeString(lexer: Lexx):Either[List[MatcherState], Option[Lexx]] = {
      lexer.next match {
        case Left(lms) =>
          println(lms.toString)
          Left(lms)
        case Right(l) if l.size > 0 =>
          //Lexx.logger.debug( f"${l.currentToken.get.tokenType}: '${l.currentToken.get.value}'" )
          l(0).currentToken.get.tokenType match {
            case "Whitespace" => whitespace += l(0).currentToken.get.length
            case "Ident" => words += 1
            case "Integer" => ints += 1
            case "Float" => floats += 1
            case "Symbol" => symbols += 1
            case _ =>
          }
          consumeString(l(0))
        case _ =>
          Right(None)
      }
    }
    consumeString(lexer)
    println(s"Whitespace: ${whitespace} Words: ${words} Symbols: ${symbols} Integers: ${ints} Floats: ${floats}")
  }
}
