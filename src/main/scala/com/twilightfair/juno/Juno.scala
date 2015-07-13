package com.twilightfair.juno

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.parse.Parser
import com.twilightfair.juno.lexx.Lexx

/**
 * Created by jthomas on 6/17/14.
 */
case class Juno(lang: Language) {

  def parse(script: Stream[Char]) = {
    Parser.parse(Parser(Lexx(lang.lexxConfig, script), lang), Nil, 0)
  }
}

object Juno {

}
