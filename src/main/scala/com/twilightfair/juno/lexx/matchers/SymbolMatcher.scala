package com.twilightfair.juno.lexx.matchers

/**
 * Created with IntelliJ IDEA.
 * User: jthomas
 * Date: 11/27/13
 * Time: 10:33 AM
 * To change this template use File | Settings | File Templates.
 */
object SymbolMatcher extends Matcher {
  
  case class Aggregator(matching: List[String], success: Option[String], failed: List[String])

  def step(start:Int, stepCount:Int, value:String, symbols: Aggregator)(text: Stream[Char], location: Int): MatcherState = {

    def nextStep(aggregate: Aggregator): MatcherState.Matching = {
      MatcherState.Matching(step(start, stepCount + 1, value + text(location), aggregate) _)
    }
    def fail(message: String) = {
      Fail("Symbol", start, stepCount, message, Map())
    }
    def success(value: String) = {
      Success("Symbol", start, stepCount, value, Map())
    }

    if (location >= text.size) {
      if (stepCount == 0)
        fail("EOF")
      else
        success(value)
    } else {
      val cTest = text(location)
      val aggregate = symbols.matching.foldLeft(Aggregator(List[String](), symbols.success, symbols.failed))(
        (agg, symbol) => {
          if (stepCount >= symbol.length) {
            Aggregator(agg.matching, Some(symbol), agg.failed)
          } else if (symbol.charAt(stepCount) != cTest) {
            Aggregator(agg.matching, agg.success, symbol :: agg.failed)
          } else {
            Aggregator(symbol :: agg.matching, agg.success, agg.failed)
          }
        }
      )
      if (aggregate.matching.length > 0){
        nextStep(aggregate)
      } else if (aggregate.success.isEmpty) {
        fail("No match.")
      } else {
        success(aggregate.success.get)
      }
    }
  }

  def init(start: Int, symbols: List[String]): MatcherState = {
    MatcherState.Matching(SymbolMatcher.step(start, 0, "", Aggregator(symbols, None, Nil)) _)
  }

  def init(start: Int): MatcherState = ???
}