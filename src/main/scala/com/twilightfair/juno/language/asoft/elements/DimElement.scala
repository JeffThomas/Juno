package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

/**
 * Created by jthomas on 12/17/13.
 */
class DimElement(config: PrefixConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val arrays: Element = config.right
  override def getValue: Any = arrays.getValue

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    proc.data("Dim") = false
    setNextElementRun(proc, proc.heap.tailSafe)
  }

  override def run(proc: Process): Process ={
    proc.data("Dim") = true
    proc.copy(runStack = arrays.run _ :: execute _ :: proc.runStack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    arrays.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"Dim${DimElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val a = arrays.graph(graph)
    val buf = new StringBuilder
    buf ++= a._2 + s"class ${graphName}\n"
    buf ++= s"${graphName} <|-- ${a._1}: arrays\n"
    (graphName, buf.toString())
  }
}

object DimElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new DimElement(config.asInstanceOf[PrefixConfig])
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}