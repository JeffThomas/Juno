package com.twilightfair.juno.lexx.matchers

import com.twilightfair.juno.lexx.{Lexx, LexxConfig}
import org.specs2.mutable.Specification

/**
 * Created by Jeff on 3/17/2015.
 */
class FloatMatcherSpec extends Specification {

  val config =  new LexxConfig {
    override def matchers(location: Int): List[MatcherState] = List(FloatMatcher.init(location), WhitespaceMatcher.init(location, true))
  }

  def testResult(result: Either[List[MatcherState],Option[Lexx]], value: String) = {
    result must beRight
    result.right.get must beSome
    result.right.get.get.currentToken must beSome
    result.right.get.get.currentToken.get.tokenType mustEqual("Float")
    result.right.get.get.currentToken.get.value mustEqual(value)
    result.right.get.get
  }

  "The IntegerMatcher" should {

    "parse a float" in {
      val lexer: Lexx = Lexx(config, "5.3".toStream)
      testResult(lexer.nextOne, "5.3").currentToken must beSome
    }

    "parse two floats" in {
      val lexer: Lexx = Lexx(config, "2.5 456.7890".toStream)
      val one = testResult(lexer.nextOne, "2.5")
      val two = one.nextOne.right.get.get // burns off the space
      testResult(two.nextOne, "456.7890").currentToken must beSome
    }

    "not get confused by leading zeros" in {
      val lexer: Lexx = Lexx(config, "000010.0".toStream)
      testResult(lexer.nextOne, "000010.0").currentToken must beSome
    }

  }
}
