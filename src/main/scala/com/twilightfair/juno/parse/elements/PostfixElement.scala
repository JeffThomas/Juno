package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.Element
import com.twilightfair.juno.runtime.Process

/**
 * Created by jthomas on 7/1/14.
 */
abstract class PostfixElement(config: ElementConfig)
  extends Element(config){

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def graph(graph: String): (String, String) = ???

  override def run(proc: Process): Process = ???

  override def getValue: Any = ???
}
