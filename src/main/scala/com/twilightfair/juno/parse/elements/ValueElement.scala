package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.parse.elements.configs.ElementConfig

/**
 * Created by jthomas on 12/17/13.
 */
class ValueElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val value: Any = config.lexx.currentToken.get.value
  override def getValue: Any = value

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process = {
    setNextElementRun(proc, Value(value, None) :: proc.heap)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $value\n"
  }

  lazy val graphName = s"Value${ValueElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class $graphName{\n\t${value.toString}\n}\n")
  }
}

object ValueElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new ValueElement(config)))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}
