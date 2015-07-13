package com.twilightfair.juno.lexx.matchers

import com.twilightfair.juno.lexx.Token

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/27/13
 * Time: 10:36 AM
 * To change this template use File | Settings | File Templates.
 */

trait MatcherState {
  def step: (Stream[Char], Int) => MatcherState
  val result: Token
}

trait Matcher {
  def init(start: Int): MatcherState
}

object MatcherState {

  case class Start(step: ((Stream[Char], Int) => MatcherState)) extends MatcherState {
    val result: Token = null
  }

  case class Matching(step: ((Stream[Char], Int) => MatcherState)) extends MatcherState {
    val result: Token = null
  }

  case class Success(result: Token) extends MatcherState {
    val step: ((Stream[Char], Int) => MatcherState) = null
  }

  case class Fail(result: Token) extends MatcherState {
    val step: ((Stream[Char], Int) => MatcherState) = null
  }
}

object Fail {
  def apply(tokenType: String, start: Int, stepCount: Int, message: String, data: Map[String, Any]) = {
    MatcherState.Fail(Token(tokenType, start, stepCount, message, false, data))
  }
}

object Success {
  def apply(tokenType: String, start: Int, stepCount: Int, value: String, data: Map[String, Any]) = {
    MatcherState.Success(Token(tokenType, start, stepCount, value, true, data))
  }
}

