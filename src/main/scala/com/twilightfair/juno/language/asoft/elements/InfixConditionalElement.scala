package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, InfixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}

/**
 * Created by jthomas on 12/17/13.
 */
class InfixConditionalElement(config: InfixConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val right: Element = config.right

  override def getValue: Any = operator

  def math(right: Float, left: Float): Boolean = {
    operator match {
      case ">" =>
        left > right
      case "<" =>
        left < right
      case "==" =>
        left == right
      case "<=" =>
        left <= right
      case ">=" =>
        left >= right
      case "<>" =>
        left != right
      case "< >" =>
        left != right
      case _ =>
        false
    }
  }

  def math(right: String, left: String): Boolean = {
    operator match {
      case ">" =>
        left > right
      case "<" =>
        left < right
      case "==" =>
        left == right
      case "<=" =>
        left <= right
      case ">=" =>
        left >= right
      case "<>" =>
        left != right
      case "< >" =>
        left != right
      case _ =>
        false
    }
  }

  def execute(proc: Process): Process = {
    val right = proc.heap.headOption
    val left = proc.heap.tailSafe.headOption
    val ret = right.flatMap { right1 =>
      left.map { left1 =>
        left1.value match {
          case li: Int => right1.value match {
            case ri: Int =>
              math(ri.toFloat, li.toFloat)
            case rf: Float =>
              math(rf, li.toFloat)
            case rs: String =>
              math(rs.toFloat, li.toFloat)
            case _ =>
              false
          }
          case lf: Float => right1.value match {
            case ri: Int =>
              math(ri.toFloat, lf)
            case rf: Float =>
              math(rf, lf)
            case rs: String =>
              math(rs.toFloat, lf)
            case _ =>
              false
          }
          case ls: String =>
            math(right1.value.toString, ls)
          case _ =>
            false
        }
      }
    }
    proc.copy(heap = Value(ret.get, None) :: proc.heap.tailSafe.tailSafe)
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = left.run _ :: right.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $operator\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"InfixMath${InfixConditionalElement.getCount}"
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

object InfixConditionalElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixConditionalElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}