package com.twilightfair.juno.lexx.matchers

import com.twilightfair.juno.lexx.{Lexx, LexxConfig}
import org.specs2.mutable.Specification

/**
 * Created by Jeff on 3/17/2015.
 */
class SymbolMatcherSpec extends Specification {

  def config(symbols: List[String])  =  new LexxConfig {
    override def matchers(location: Int): List[MatcherState] = List(SymbolMatcher.init(location, symbols), WhitespaceMatcher.init(location, true))
  }

  def testResult(result: Either[List[MatcherState],Option[Lexx]], value: String) = {
    result must beRight
    result.right.get must beSome
    result.right.get.get.currentToken must beSome
    result.right.get.get.currentToken.get.tokenType mustEqual("Symbol")
    result.right.get.get.currentToken.get.value mustEqual(value)
    result.right.get.get
  }

  "The SymbolMatcher" should {

    "parse a symbol" in {
      val symbols: List[String] = List("+")
      val lexer: Lexx = Lexx(config(symbols), "+".toStream)
      testResult(lexer.nextOne, "+").currentToken must beSome
    }

    "parse two symbols" in {
      val symbols: List[String] = List("++", "*")
      val lexer: Lexx = Lexx(config(symbols), "* ++".toStream)
      val one = testResult(lexer.nextOne, "*")
      val two = one.nextOne.right.get.get // burns off the space
      testResult(two.nextOne, "++").currentToken must beSome
    }

    "parse two sized symbols" in {
      val symbols: List[String] = List("++", "+++")
      val lexer: Lexx = Lexx(config(symbols), "+++ ++".toStream)
      val one = testResult(lexer.nextOne, "+++")
      val two = one.nextOne.right.get.get // burns off the space
      testResult(two.nextOne, "++").currentToken must beSome
    }
  }
}
