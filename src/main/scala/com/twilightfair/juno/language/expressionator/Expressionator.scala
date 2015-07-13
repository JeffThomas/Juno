package com.twilightfair.juno.language.expressionator

import com.twilightfair.juno.Constants._
import com.twilightfair.juno.language.Language
import com.twilightfair.juno.language.expressionator.elements._
import com.twilightfair.juno.lexx.{LexxConfig, Lexx}
import com.twilightfair.juno.lexx.matchers._
import com.twilightfair.juno.parse.parsers._
import com.twilightfair.juno.parse.{Out, Parser, TokenParser}
import com.twilightfair.juno.runtime.{Runner, Process}

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * Created by jthomas on 12/11/13.
 */

object Expressionator extends Language with Runner {

  def symbols: List[String] =
    List("+", "-", "/", "*", "(", ")", "++", ",")

  def keywords: List[String] =
    Nil

  override val lexxConfig = new LexxConfig {
    def matchers(location: Int): List[MatcherState] =
      List(
        SymbolMatcher.init(location, symbols),
        IntegerMatcher.init(location),
        WhitespaceMatcher.init(location, true),
        FloatMatcher.init(location)
      )
  }

  override val prefixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Integer", None, LeafParser(IntegerElementRPN, PRECEDENCE_PREFIX)),
      ("Float", None, LeafParser(FloatElementRPN, PRECEDENCE_PREFIX)),
      ("Symbol", Some("-"), PrefixParser(PrefixMathElementRPN, PRECEDENCE_PREFIX)),
      ("Symbol", Some("("), BlockParser(BlockElementRPN, PRECEDENCE_PREFIX)),
      ("Symbol", Some(")"), BlockEndParser(BlockElementRPN, PRECEDENCE_PREFIX))
  )

  override val infixTokens: List[(String, Option[String], TokenParser)] =
    List(
      ("Symbol", Some("+"), InfixParser(InfixMathElementRPN, PRECEDENCE_SUM)),
      ("Symbol", Some("-"), InfixParser(InfixMathElementRPN, PRECEDENCE_SUM)),
      ("Symbol", Some("*"), InfixParser(InfixMathElementRPN, PRECEDENCE_PRODUCT)),
      ("Symbol", Some("/"), InfixParser(InfixMathElementRPN, PRECEDENCE_PRODUCT)),
      ("Symbol", Some("++"), PostfixParser(PostfixMathElementRPN, PRECEDENCE_POSTFIX)),
      ("Symbol", Some(","), InfixParser(InfixSeparatorElementRPN, PRECEDENCE_ASSIGNMENT))
    )

  @tailrec override final def runExpression(proc: Process): Future[Process] = {
    if (proc.runStack.headOption.isEmpty)
      Future.successful(proc)
    else
      runExpression(proc.runStack.headOption.get(proc.copy(runStack = proc.runStack.tail)))
  }
}
