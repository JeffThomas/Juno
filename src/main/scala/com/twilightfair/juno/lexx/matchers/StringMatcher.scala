package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 12/2/13
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */
object StringMatcher extends Matcher {

  def step(start:Int, stepCount:Int, value:String)(text: Stream[Char], location: Int): MatcherState = {
    def nextMatch: MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location)))
    }
    def fail(message: String) = {
      Fail("String", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("String", start, stepCount + 1, value, Map())
    }

    if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        fail("Unclosed string!")
    } else {
      val cTest = text(location)
      cTest match {
        case c if stepCount == 0 =>
          if (c.equals('\"'))
            nextMatch
          else
            fail("No opening quote")
        case '\"' if stepCount >= 1 =>
          success(value.substring(1))
        case _ =>
          nextMatch
      }
    }
  }

  override def init(start: Int): MatcherState = {
    MatcherState.Matching(StringMatcher.step(start, 0, ""))
  }

}
