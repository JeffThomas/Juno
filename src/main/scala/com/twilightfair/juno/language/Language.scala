package com.twilightfair.juno.language

import com.twilightfair.juno.Constants._
import com.twilightfair.juno.lexx.{EmptyLexxConfig, LexxConfig, Lexx}
import com.twilightfair.juno.parse._
import com.twilightfair.juno.runtime.Process

import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * Created by jthomas on 12/11/13.
 */

trait Language {

  val lang: Language = this

  def prefixTokens: List[(String, Option[String], TokenParser)]
  def infixTokens: List[(String, Option[String], TokenParser)]

  val lexxConfig: LexxConfig

  def parse(script: Stream[Char]): Out = {
    Parser.parse(buildParser(Lexx(lexxConfig, script)), Nil, PRECEDENCE_NONE)
  }

  def buildParser(lexer: Lexx): Parser = {
    Parser(lexer, this)
  }

  def preElement(token: Parser.ParseLex, data: HashMap[String, Any]): (TokenParser, HashMap[String, Any]) = {
    (token.parser, data)
  }
}

object Language {
  object EmptyLang extends Language {

    override def parse(script: Stream[Char]) = {
      Parser.parse(Parser(Lexx(lexxConfig, script), this), Nil, 0)
    }

    override val lexxConfig: LexxConfig = new EmptyLexxConfig{}

    override val prefixTokens: List[(String, Option[String], TokenParser)] = ???

    override val infixTokens: List[(String, Option[String], TokenParser)] = ???

    override def buildParser(lexer: Lexx): Parser = ???
  }
}
