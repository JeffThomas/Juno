package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/27/13
 * Time: 10:33 AM
 * To change this template use File | Settings | File Templates.
 */
object IdentifierMatcher extends Matcher {

  def step(start:Int, stepCount:Int, value:String, keywords:List[String], symbols: String, tailSpecials: String, end: Boolean = false)(text: Stream[Char], location: Int): MatcherState = {

    def nextStep(end: Boolean): MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location), keywords, symbols, tailSpecials, end))
    }
    def fail(message: String) = {
      Fail("Ident", start, stepCount, message, Map())
    }
    def success(value: String) = {
      if (keywords.contains(value))
        Success("Keyword", start, stepCount, value, Map())
      else
        Success("Ident", start, stepCount, value, Map())
    }

    if (end) {
      success(value)
    } else if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        success(value)
    } else {
      val cTest = text(location)
      cTest match {
        case _ if stepCount > 0 && tailSpecials.contains(cTest) =>
          nextStep(true)
        case _ if stepCount > 0 && (cTest.isLetter | cTest.isDigit | symbols.contains(cTest)) =>
          nextStep(false)
        case _ if stepCount == 0 && (cTest.isLetter | symbols.contains(cTest)) =>
          nextStep(false)
        case _ =>
          if (stepCount > 0)
            success(value)
          else
            fail("Does not start with _ or a letter")
      }
    }
  }

  def init(start: Int, keywords: List[String], symbols: String, tailSpecials: String): MatcherState = {
    MatcherState.Matching(IdentifierMatcher.step(start, 0, "", keywords, symbols, tailSpecials) _)
  }

  def init(start: Int): MatcherState = ???
}