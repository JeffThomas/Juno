package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/27/13
 * Time: 10:33 AM
 * To change this template use File | Settings | File Templates.
 */
object IntegerMatcher extends Matcher {

  def step(start:Int, stepCount:Int, value:String)(text: Stream[Char], location: Int): MatcherState = {
    def nextStep: MatcherState.Matching = MatcherState.Matching(step(start, stepCount + 1, value + text(location)) _)
    def fail(message: String) = {
      Fail("Integer", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("Integer", start, stepCount, value, Map())
    }

    if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        success(value)
    } else {
      val cTest = text(location)
      cTest match {
        case _ if cTest.isDigit =>
          nextStep
        case _ if cTest.isLetter | stepCount == 0 =>
          fail("Non number found")
        case _ =>
          success(value)
      }
    }
  }

  override def init(start: Int): MatcherState = {
    MatcherState.Matching(IntegerMatcher.step(start, 0, "") _)
  }
}