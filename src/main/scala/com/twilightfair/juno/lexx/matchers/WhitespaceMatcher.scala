package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 12/3/13
 * Time: 1:33 PM
 * To change this template use File | Settings | File Templates.
 */
object WhitespaceMatcher extends Matcher {

  def step(start: Int, stepCount: Int, value:String, ret: Boolean)(text: Stream[Char], location: Int): MatcherState = {
    def nextStep: MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location), ret))
    }
    def fail(message: String) = {
      Fail("Whitespace", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("Whitespace", start, stepCount, value, Map())
    }

    if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        success(value)
    } else {
      val cTest = text(location)
      cTest match {
        case _ if cTest.isWhitespace && (ret || (cTest != '\n' && cTest != '\r')) =>
          nextStep
        case _ if stepCount == 0 =>
          fail("No whitespace")
        case _ =>
          success(value)
      }
    }
  }

  def init(start: Int, ret: Boolean): MatcherState = {
    MatcherState.Matching(WhitespaceMatcher.step(start, 0, "", ret))
  }

  def init(start: Int): MatcherState = ???

}