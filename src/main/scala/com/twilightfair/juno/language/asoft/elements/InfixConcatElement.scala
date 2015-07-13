package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, InfixConfig}
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.asoft.elements.configs.InfixConcatConfig

/**
 * Created by jthomas on 12/17/13.
 */
class InfixConcatElement(config: InfixConcatConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val rightOpt: Option[Element] = config.right

  override def getValue: Any = operator

  def execute(proc: Process): Process = {
    if (rightOpt.isDefined){
      val rightEl = proc.heap.headOption
      val leftEl = proc.heap.tailSafe.headOption
      val ret = rightEl.flatMap {
        right1 =>
          leftEl.map {
            left1 =>
              left1.value.toString + right1.value.toString
          }
      }
      proc.copy(heap = Value(ret.get, None) :: proc.heap.tailSafe.tailSafe)
    } else {
      val leftEl = proc.heap.headOption
      val ret = leftEl.map{ _.value.toString }
      proc.copy(heap = Value(ret.get, None) :: Value(";", None) :: proc.heap.tailSafe) // not super happy about this
    }
  }

  override def run(procCurrent: Process): Process = {
    val stackNext = execute _ :: setNext(procCurrent.runStack)
    val stack = left.run _ :: (if (rightOpt.isDefined) rightOpt.get.run _ :: stackNext else stackNext)
    procCurrent.copy(runStack = stack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $operator\n"
    left.tree(indent+"  ", buffer)
    rightOpt.map(
      _.tree(indent+"  ", buffer)
    ).getOrElse(buffer)
  }

  lazy val graphName = s"InfixConcat${InfixConcatElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val r = rightOpt.map(_.graph(l._2))
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r.getOrElse("",l._2)._2 + s"class ${graphName}{\n\t${op}\n}\n"
    buf ++= s"${graphName} <|-- ${l._1}: left\n"
    if (r.isDefined) buf ++= s"${graphName} <|-- ${r.get._1}: right\n"
    (graphName, buf.toString())
  }
}

object InfixConcatElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixConcatElement(config.asInstanceOf[InfixConcatConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}