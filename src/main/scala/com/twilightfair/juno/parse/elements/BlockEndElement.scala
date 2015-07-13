package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.{Out, Element, ElementFactory}
import com.twilightfair.juno.parse.elements.configs.{BlockConfig, ElementConfig}
import com.twilightfair.juno.runtime.Process
import com.twilightfair.juno.language.expressionator.elements.FloatElement

/**
 * Created by jthomas on 12/17/13.
 */
class BlockEndElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  var value: String = config.lexx.currentToken.get.value

  override def getValue: Any = value

  override def run(env: Process): Process = ???

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $value\n"
  }

  lazy val graphName = s"BlockEnd${BlockEndElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class ${graphName}{\n${value}\n}\n")
  }
}

object BlockEndElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new BlockEndElement(config)))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}