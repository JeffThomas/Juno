package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.ElementFactory
import com.twilightfair.juno.parse.elements.PrefixElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.runtime.Process

/**
 * Created by jthomas on 7/1/14.
 */
class OnElement(config: PrefixConfig)
  extends PrefixElement(config){

  override def run(proc: Process): Process = {
    proc.copy(runStack = right.run _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
  }

  override lazy val graphName = s"On${OnElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = right.graph(graph)
    val buf = new StringBuilder
    buf ++= r._2 + s"class ${graphName}\n"
    buf ++= s"${graphName} <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object OnElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    Right(Some(new OnElement(config.asInstanceOf[PrefixConfig])))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}