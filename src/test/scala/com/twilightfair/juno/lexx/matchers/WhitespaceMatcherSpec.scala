package com.twilightfair.juno.lexx.matchers

import com.twilightfair.juno.lexx.{Lexx, LexxConfig}
import org.specs2.mutable.Specification

/**
 * Created by Jeff on 3/17/2015.
 */
class WhitespaceMatcherSpec extends Specification {

  def config(ret: Boolean)  =  new LexxConfig {
    override def matchers(location: Int): List[MatcherState] = List(WhitespaceMatcher.init(location, ret))
  }

  def testResult(result: Either[List[MatcherState],Option[Lexx]], value: String) = {
    result must beRight
    result.right.get must beSome
    result.right.get.get.currentToken must beSome
    result.right.get.get.currentToken.get.tokenType mustEqual("Whitespace")
    result.right.get.get.currentToken.get.value mustEqual(value)
    result.right.get.get
  }

  "The SymbolMatcher" should {

    "parse a symbol" in {
      val lexer: Lexx = Lexx(config(true), " ".toStream)
      testResult(lexer.nextOne, " ").currentToken must beSome
    }

    "parse two symbols" in {
      val lexer: Lexx = Lexx(config(true), " \n".toStream)
      testResult(lexer.nextOne, " \n").currentToken must beSome
    }

    "parse two sized symbols" in {
      val lexer: Lexx = Lexx(config(false), " \n".toStream)
      val one = testResult(lexer.nextOne, " ")
      val two = one.nextOne
      two must beLeft
    }
  }
}
