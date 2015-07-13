package com.twilightfair.juno.language.asoft

import com.twilightfair.juno.lexx.{Lexx, LexxConfig}
import com.twilightfair.juno.lexx.matchers.{WhitespaceMatcher, IntegerMatcher, MatcherState}
import org.specs2.mutable.Specification

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Created by Jeff on 3/17/2015.
 */
class IntegerMatcherSpec extends Specification {

  val config =  new LexxConfig {
    override def matchers(location: Int): List[MatcherState] = List(IntegerMatcher.init(location), WhitespaceMatcher.init(location, true))
  }

  def testResult(result: Either[List[MatcherState],Option[Lexx]], value: String) = {
    result must beRight
    result.right.get must beSome
    result.right.get.get.currentToken must beSome
    result.right.get.get.currentToken.get.tokenType mustEqual("Integer")
    result.right.get.get.currentToken.get.value mustEqual(value)
    result.right.get.get
  }

  "The IntegerMatcher" should {

    "parse an integer" in {
      val lexer: Lexx = Lexx(config, "5".toStream)
      testResult(lexer.nextOne, "5").currentToken must beSome
    }

    "parse two integers" in {
      val lexer: Lexx = Lexx(config, "5 54321".toStream)
      val one = testResult(lexer.nextOne, "5")
      val two = one.nextOne.right.get.get // burns off the space
      testResult(two.nextOne, "54321").currentToken must beSome
    }

    "not be fooled by a float" in {
      val lexer: Lexx = Lexx(config, "983764.54333".toStream)
      testResult(lexer.nextOne, "983764").currentToken must beSome
    }

    "not get confused by leading zeros" in {
      val lexer: Lexx = Lexx(config, "000010".toStream)
      testResult(lexer.nextOne, "000010").currentToken must beSome
    }

  }
}
