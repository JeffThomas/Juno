package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.elements.configs.{PrefixConfig, ElementConfig}
import com.twilightfair.juno.parse.elements.PrefixElement
import com.twilightfair.juno.runtime.{Value, Process}
import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

/**
 * Created by jthomas on 7/1/14.
 */
class PrintElement(config: PrefixConfig)
  extends PrefixElement(config) {

  var printReturn: Boolean = true

  override def execute(proc: Process): Process = {
    val right = proc.heap.headOption
    val tail = proc.heap.tailSafe
    if(right.isDefined) {
      val out = s"${right.get.value.toString}"
      if (!tail.isEmpty && tail.head.value.isInstanceOf[String] && tail.head.value == ";") {
        proc.copy(heap = tail.tailSafe, outBuffer = out :: proc.outBuffer)
      } else {
        if (printReturn) {
          proc.copy(heap = tail, outBuffer = "\n" :: out :: proc.outBuffer)
        } else {
          proc.copy(heap = tail, outBuffer = out :: proc.outBuffer)
        }
      }
    } else {
      proc.copy(heap = tail, outBuffer = "\n" :: proc.outBuffer)
    }
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = right.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    right.tree(indent+"  ", buffer)
  }

  override lazy val graphName = s"Print${PrintElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = right.graph(graph)
    val buf = new StringBuilder
    buf ++= r._2 + s"class ${graphName}\n"
    buf ++= s"${graphName} <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object PrintElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val pe = new PrintElement(config.asInstanceOf[PrefixConfig])
    config.data.get("PrintCr").map{ printCrAny =>
      pe.printReturn = printCrAny.asInstanceOf[Boolean]
    }
    Right(Some(pe))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}