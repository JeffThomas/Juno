package com.twilightfair.juno.language.test

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.language.expressionator.elements.{FloatElement, InfixMathElement, IntegerElement}
import com.twilightfair.juno.lexx.{LexxConfig, Lexx}
import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.parsers.{InfixParser, LeafParser}
import com.twilightfair.juno.parse.{Out, Parser, TokenParser}
import com.twilightfair.juno.runtime.Process

import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * Created by jthomas on 12/11/13.
 */
object BookReader extends Language {

  def symbols: List[String] =
    List("+", "++", "-", "--", "/", "*", "=", "==", "(", ")",
      "'", ".", ",", "[", "]", "#", ":", "?", "!", ";", "\"", "%", "@", "$")


  def keywords: List[String] =
    List("for", "if", "else", "switch", "case", "while")


  override val lexxConfig = new LexxConfig {
    def matchers(location: Int): List[MatcherState] =
      List(
        IdentifierMatcher.init(location, keywords, "", ""),
        SymbolMatcher.init(location, symbols),
        IntegerMatcher.init(location),
        WhitespaceMatcher.init(location, true),
        FloatMatcher.init(location)
      )
  }

  override val prefixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Integer", None, LeafParser(IntegerElement, 1)),
      ("Float", None, LeafParser(FloatElement, 1))
  )

  override val infixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Symbol", Some("+"), InfixParser(InfixMathElement, 1))
    )

  override def buildParser(lexer: Lexx): Parser = ???

}
