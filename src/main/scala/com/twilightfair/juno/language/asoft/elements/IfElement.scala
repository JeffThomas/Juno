package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.language.expressionator.elements.{FloatElement, IntegerElement}
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

import scala.collection.mutable

/**
 * Created by jthomas on 12/17/13.
 */
class IfElement(config: PrefixConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val condition: Element = config.right
  val value: String = config.lexx.currentToken.get.value
  override def getValue: Any = value

  var skipTo: Option[Element] = None

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    val c = proc.heap.headOption
    if (c.isInstanceOf[Some[Value]] && c.asInstanceOf[Some[Value]].get.value.asInstanceOf[Boolean]) {
      setNextElementRun(proc, proc.heap.tailSafe)
    } else {
      skipTo.map { skip =>
        proc.copy(runStack = List(skipTo.get.run _), heap = proc.heap.tailSafe)
      }.getOrElse {
        proc.copy(runStack = Nil, heap = proc.heap.tailSafe)
      }
    }
  }

  override def run(proc: Process): Process ={
    proc.copy(runStack = condition.run _ :: execute _ :: proc.runStack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $value\n"
    condition.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"If${IfElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val buf = new StringBuilder
    buf ++= graph
    buf ++= s"package ${graphName}{\n"
    buf ++= condition.graph("")._2
    buf ++= "}\n"
    buf ++= graphLinkNext("then")
    (graphName, buf.toString())
  }
}

object IfElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new IfElement(config.asInstanceOf[PrefixConfig])
    config.data("If") = element :: config.data("If").asInstanceOf[List[IfElement]]
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}