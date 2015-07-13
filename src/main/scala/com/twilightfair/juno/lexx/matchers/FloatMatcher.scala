package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/27/13
 * Time: 10:33 AM
 * To change this template use File | Settings | File Templates.
 */
object FloatMatcher extends Matcher {

  def step(start:Int, stepCount:Int, value:String, point: Boolean)(text: Stream[Char], location: Int): MatcherState = {
    def nextStep(pointChange: Boolean): MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location), pointChange) _)
    }
    def fail(message: String) = {
      Fail("Float", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("Float", start, stepCount, value, Map())
    }

    if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        if (point && stepCount > 1 && !text.endsWith("."))
          success(value)
        else
          fail("no point")
    } else {
      val cTest = text(location)
      cTest match {
        case _ if cTest.isDigit =>
          nextStep(point)
        case '.' if stepCount == 0 =>
          nextStep(true)
        case _ if cTest.isLetter | stepCount == 0 =>
          fail("Letter or nothing found")
        case '.' =>
          if (point)
            success(value)
          else
            nextStep(true)
        case _ =>
          if (point && stepCount > 1 && text(location-1) != '.')
            success(value)
          else
            fail("No decimal or no number")
      }
    }
  }

  override def init(start: Int): MatcherState = {
    MatcherState.Matching(FloatMatcher.step(start, 0, "", false) _)
  }
}