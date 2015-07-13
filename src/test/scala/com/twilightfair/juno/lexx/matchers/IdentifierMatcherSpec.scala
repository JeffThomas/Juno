package com.twilightfair.juno.lexx.matchers

import com.twilightfair.juno.lexx.{Lexx, LexxConfig}
import org.specs2.mutable.Specification

/**
 * Created by Jeff on 3/17/2015.
 */
class IdentifierMatcherSpec extends Specification {

  def config(keywords: List[String])  =  new LexxConfig {
    override def matchers(location: Int): List[MatcherState] = List(IdentifierMatcher.init(location, keywords, "_", "$%"), WhitespaceMatcher.init(location, true))
  }

  def testResult(result: Either[List[MatcherState],Option[Lexx]], iType: String, value: String) = {
    result must beRight
    result.right.get must beSome
    result.right.get.get.currentToken must beSome
    result.right.get.get.currentToken.get.tokenType mustEqual(iType)
    result.right.get.get.currentToken.get.value mustEqual(value)
    result.right.get.get
  }

  "The SymbolMatcher" should {

    "parse an identifier" in {
      val keywords: List[String] = List()
      val lexer: Lexx = Lexx(config(keywords), "x".toStream)
      testResult(lexer.nextOne, "Ident", "x").currentToken must beSome
    }

    "parse identifiers and keywords" in {
      val keywords: List[String] = List("for", "next")
      val lexer: Lexx = Lexx(config(keywords), "count for you".toStream)
      val one = testResult(lexer.nextOne, "Ident", "count")
      val two = one.nextOne.right.get.get // burns off the space
      val three = testResult(two.nextOne, "Keyword", "for")
      val four = three.nextOne.right.get.get // burns off the space
      testResult(four.nextOne, "Ident", "you").currentToken must beSome
    }

    "parse mixed keywords and identifiers" in {
      val keywords: List[String] = List("fornext", "next")
      val lexer: Lexx = Lexx(config(keywords), "nextone for next".toStream)
      val one = testResult(lexer.nextOne, "Ident", "nextone")
      val two = one.nextOne.right.get.get // burns off the space
      val three = testResult(two.nextOne, "Ident", "for")
      val four = three.nextOne.right.get.get // burns off the space
      testResult(four.nextOne, "Keyword", "next").currentToken must beSome
    }

    "parse underscored identifiers" in {
      val keywords: List[String] = List()
      val lexer: Lexx = Lexx(config(keywords), "_this next_1_to _you_".toStream)
      val one = testResult(lexer.nextOne, "Ident", "_this")
      val two = one.nextOne.right.get.get // burns off the space
      val three = testResult(two.nextOne, "Ident", "next_1_to")
      val four = three.nextOne.right.get.get // burns off the space
      testResult(four.nextOne, "Ident", "_you_").currentToken must beSome
    }

    "parse other tail symbols" in {
      val keywords: List[String] = List()
      val lexer: Lexx = Lexx(config(keywords), "this_ next_1_to$ _you$".toStream)
      val one = testResult(lexer.nextOne, "Ident", "this_")
      val two = one.nextOne.right.get.get // burns off the space
      val three = testResult(two.nextOne, "Ident", "next_1_to$")
      val four = three.nextOne.right.get.get // burns off the space
      testResult(four.nextOne, "Ident", "_you$").currentToken must beSome
    }

    "not fooled by embedded tail" in {
      val keywords: List[String] = List("hello", "world")
      val lexer: Lexx = Lexx(config(keywords), "_enjoy this$one".toStream)
      val one = testResult(lexer.nextOne, "Ident", "_enjoy")
      val two = one.nextOne.right.get.get // burns off the space
      testResult(two.nextOne, "Ident", "this$").currentToken must beSome
    }

    "fail on number start" in {
      val keywords: List[String] = List("hello", "world")
      val lexer: Lexx = Lexx(config(keywords), "a1 1a".toStream)
      val one = testResult(lexer.nextOne, "Ident", "a1")
      val two = one.nextOne.right.get.get // burns off the space
      two.nextOne must beLeft
    }

    "fail on tail start" in {
      val keywords: List[String] = List("hello", "world")
      val lexer: Lexx = Lexx(config(keywords), "this $money".toStream)
      val one = testResult(lexer.nextOne, "Ident", "this")
      val two = one.nextOne.right.get.get // burns off the space
      two.nextOne must beLeft
    }
  }
}
