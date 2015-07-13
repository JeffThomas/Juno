package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.expressionator.elements.FloatElement
import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

import scala.collection.mutable.ArrayBuffer

/**
 * Created by jthomas on 12/17/13.
 */
class ReturnElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val value: String = config.lexx.currentToken.get.value
  override def getValue: Any = value

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def run(proc: Process): Process = {
    val tOption = proc.data("CallStack").asInstanceOf[ArrayBuffer[Element]].headOption
    proc.data("CallStack").asInstanceOf[ArrayBuffer[Element]].clear() // no nested GOSUBs
    tOption.flatMap { t =>
      if (t.isInstanceOf[Element]) {
        Some(proc.copy(runStack = List(t.run _), heap = Nil))
      } else {
        println(s"Invalid RETURN target at line ${info("row")}")
        None
      }
    }.getOrElse{
      println(s"Nothing to RETURN to at line ${info("row")}")
      proc.copy(runStack = proc.runStack.tail, heap = proc.heap.tailSafe)
    }
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $value\n"
  }

  lazy val graphName = s"Return${ReturnElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val ret = s"class $graphName{\n\t$value\n}\n"
    (graphName, graph + ret)
  }
}

object ReturnElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new ReturnElement(config)
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}