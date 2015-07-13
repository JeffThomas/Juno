package com.twilightfair.juno.language.asoft.matchers

import com.twilightfair.juno.lexx.matchers._

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 12/2/13
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */
object UnquotedStringMatcher extends Matcher {
  def step(start:Int, stepCount:Int, value:String)(text: Stream[Char], location: Int): MatcherState = {
    def nextMatch: MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location)))
    }
    def fail(message: String) = {
      Fail("String", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("String", start, stepCount, value, Map())
    }

    if (location >= text.size) {
      success(value)
    } else {
      val cTest = text(location)
      cTest match {
        case _ if cTest == '\n' || cTest == '\r' =>
          success(value)
        case ',' if stepCount >= 0 =>
          success(value)
        case '"' =>
          fail("has a quote")
        case _ =>
          nextMatch
      }
    }
  }

  override def init(start: Int): MatcherState = {
    MatcherState.Matching(UnquotedStringMatcher.step(start, 0, ""))
  }
}
