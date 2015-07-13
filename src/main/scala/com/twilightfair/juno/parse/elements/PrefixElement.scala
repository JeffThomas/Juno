package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.elements.configs.{PrefixConfig, ElementConfig}
import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.runtime.{Value, Process}

/**
 * Created by jthomas on 7/1/14.
 */
class PrefixElement(config: PrefixConfig)
  extends Element(config){

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  val operator: String = config.lexx.currentToken.get.value
  val right: Element = config.right

  def execute(proc: Process): Process = {
    proc.copy(heap = Value(operator + proc.heap.head.value.toString, None) :: proc.heap.tail)
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = right.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"Prefix${PrefixElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = right.graph(graph)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r._2 + s"class $graphName{\n\t$op\n}\n"
    buf ++= s"$graphName <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }

  override var next: Option[Element] = None

  override def getValue: Any = operator
}

object PrefixElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new PrefixElement(config.asInstanceOf[PrefixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}