package com.twilightfair.juno.language.test

import com.twilightfair.juno.Constants._
import com.twilightfair.juno.language.Language
import com.twilightfair.juno.lexx.{LexxConfig, Lexx}
import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.parsers.{InfixParser, LeafParser}
import com.twilightfair.juno.parse.{Out, Parser, TokenParser}
import com.twilightfair.juno.runtime.{Runner, Process}

import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * Created by jthomas on 12/11/13.
 */

object Tester extends Language {

  def symbols: List[String] =
    List("+", "++", "-", "--", "/", "*", "=", "==", "(", ")")

  def keywords: List[String] =
    List("for", "if", "else", "switch", "case", "while")

  override val lexxConfig = new LexxConfig {
    def matchers(location: Int): List[MatcherState] =
      List(
        IdentifierMatcher.init(location, keywords, "", ""),
        SymbolMatcher.init(location, symbols),
        IntegerMatcher.init(location),
        StringMatcher.init(location),
        WhitespaceMatcher.init(location, true),
        FloatMatcher.init(location)
      )
  }

  override val prefixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Integer", None, LeafParser(IntegerElementRPN, 1)),
      ("Float", None, LeafParser(FloatElementRPN, 1))
  )

  override val infixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Symbol", Some("+"), InfixParser(InfixMathElementRPN, PRECEDENCE_SUM)),
      ("Symbol", Some("*"), InfixParser(InfixMathElementRPN, PRECEDENCE_PRODUCT))
    )

  def main(args: Array[String]):Unit = {
    val test = "5 +\n7 * 4\n +\n 1"
    val lexer: Lexx = Lexx(lexxConfig, test.toStream)

    val results = Parser.parse(Parser(lexer, this), Nil, 0)
    results.elements map { element =>
      println(element.asInstanceOf[ElementRPN].rpn(""))
    }
  }

  override def buildParser(lexer: Lexx): Parser = ???
}
