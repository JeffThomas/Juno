package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.asoft.elements.configs.InfixConcatConfig
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.parse.elements.configs.{InfixConfig, ElementConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}

/**
 * Created by jthomas on 12/17/13.
 */
class CommaConcatElement(config: InfixConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val right: Element = config.right

  override def getValue: Any = operator

  def execute(proc: Process): Process = {
    val rightEl = proc.heap.headOption
    val leftEl = proc.heap.tailSafe.headOption
    val ret = rightEl.flatMap {
      right1 =>
        val r = right1.value match {
          case rl: List[Any] => rl
          case _ => List(right1)
        }
        leftEl.map { left1 =>
          left1.value match {
            case list: List[Any] =>
              list ::: r
            case _ =>
              left1 :: r
          }
        }
    }
    proc.copy(heap = Value(ret.get, None) :: proc.heap.tailSafe.tailSafe)
  }

  override def run(procCurrent: Process): Process = {
    val stackNext = execute _ :: setNext(procCurrent.runStack)
    val stack = left.run _ :: right.run _ :: stackNext
    procCurrent.copy(runStack = stack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"CommaConcat${CommaConcatElement.getCount}"
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

object CommaConcatElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new CommaConcatElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}