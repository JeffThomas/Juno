package com.twilightfair.juno.language.expressionator.elements

import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, InfixConfig}
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

/**
 * Created by jthomas on 12/17/13.
 */
class InfixSeparatorElement(config: InfixConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val right: Element = config.right

  override def getValue: Any = operator

  def execute(proc: Process): Process = {
    if (operator == ";")
    // pull the left results from the stack
      proc.copy(heap = proc.heap.head :: proc.heap.tailSafe.tailSafe)
    else
    // leave left and right results in the stack
      proc
  }

  override def run(procCurrent: Process): Process = {
    procCurrent.copy(runStack = left.run _ :: right.run _ :: execute _ :: setNext(procCurrent.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"InfixSeparator${InfixSeparatorElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val r = right.graph(l._2)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r._2 + s"class $graphName{\n\t$op\n}\n"
    buf ++= s"$graphName <|-- ${l._1}: left\n"
    buf ++= s"$graphName <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object InfixSeparatorElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixSeparatorElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0

  def getCount = {
    count += 1
    count
  }
}