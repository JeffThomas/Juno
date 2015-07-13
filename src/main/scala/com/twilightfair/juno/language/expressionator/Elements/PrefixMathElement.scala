package com.twilightfair.juno.language.expressionator.elements

import com.twilightfair.juno.parse.elements.configs.{PrefixConfig, ElementConfig}
import com.twilightfair.juno.parse.elements.PrefixElement
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.parse.{ElementFactory, Element}

/**
 * Created by jthomas on 7/1/14.
 */
class PrefixMathElement(config: PrefixConfig)
  extends PrefixElement(config){

  def math(right: Float): Float = {
    operator match {
      case "+" =>
        +right
      case "-" =>
        -right
      case _ =>
        0.0f
    }
  }

  override def execute(proc: Process): Process = {
    val right = proc.heap.headOption
    val ret = right.map { right1 =>
      right1.value match {
        case ri: Int =>
          math(ri.toFloat).toInt
        case rf: Float =>
          math(rf)
        case rs: String =>
          math(rs.toFloat)
        case _ =>
          0
      }
    }
    proc.copy(heap = Value(ret.get, None) :: proc.heap.tail)
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = right.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    right.tree(indent+"  ", buffer)
  }

  override lazy val graphName = s"PrefixMath${PrefixMathElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = right.graph(graph)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r._2 + s"class $graphName{\n\t$op\n}\n"
    buf ++= s"$graphName <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object PrefixMathElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new PrefixMathElement(config.asInstanceOf[PrefixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}