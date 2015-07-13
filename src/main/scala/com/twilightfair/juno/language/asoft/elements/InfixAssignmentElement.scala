package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.language.asoft.elements.traits.AssignsVariables
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.parse.elements.configs.{InfixConfig, ElementConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class InfixAssignmentElement(config: InfixConfig)
  extends InfixElement(config) with AssignsVariables {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val right: Element = config.right

  override def getValue: Any = operator

  def execute(proc: Process): Process = {
    val nameSpace: HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
    val rightOpt = proc.heap.headOption
    val leftOpt = proc.heap.tailSafe.headOption

    assignVariable(leftOpt, rightOpt, nameSpace)

    proc.copy(heap = proc.heap.tailSafe.tailSafe)
  }

  override def run(procCurrent: Process): Process = {
    val stackNext = execute _ :: setNext(procCurrent.runStack)
    val stack = left.run _ ::  right.run _ :: stackNext
    procCurrent.copy(runStack = stack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $operator\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"InfixAssignment${InfixAssignmentElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val r = right.graph(l._2)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r._2 + s"class ${graphName}{\n\t${op}\n}\n"
    buf ++= s"${graphName} <|-- ${l._1}: left\n"
    buf ++= s"${graphName} <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object InfixAssignmentElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixAssignmentElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}