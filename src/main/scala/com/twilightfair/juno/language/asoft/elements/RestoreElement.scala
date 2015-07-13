package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

/**
 * Created by jthomas on 7/8/14.
 */
class RestoreElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  override def getValue: Any = ???

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process = {
    proc.data("ReadIndex") = 0
    setNextElementRun(proc, Nil) // empty the heap
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
  }

  lazy val graphName = s"RESTORE${RestoreElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class ${graphName}\n")
  }
}

object RestoreElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val el = new RestoreElement(config)
    Right(Some(el))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}