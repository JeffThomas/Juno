package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.runtime

/**
 * Created by jthomas on 12/17/13.
 */
abstract class InfixElement(config: ElementConfig)
  extends Element(config) {

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def graph(graph: String): (String, String) = ???

  override def run(proc: runtime.Process): runtime.Process = ???

  override def getValue: Any = ???
}

object InfixElement extends ElementFactory {
  def apply(config: ElementConfig) = ???
}