package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

/**
 * Created by jthomas on 7/8/14.
 */
class SeperatorElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  override def getValue: Any = ???

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process = {
    proc.copy(runStack = setNext(proc.runStack), heap = Nil)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
  }

  lazy val graphName = s"Seperator${SeperatorElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class ${graphName}\n")
  }
}

object SeperatorElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val el = new SeperatorElement(config)
    config.data("On") = false
    Right(Some(el))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}