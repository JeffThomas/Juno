package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.runtime.{Value, Process}

/**
 * Created by jthomas on 12/17/13.
 */
class StringElement(config: ElementConfig)
  extends ValueElement(config) {

  override val value: String = config.lexx.currentToken.get.value

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $value\n"
  }

  override lazy val graphName = s"String${StringElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class ${graphName}{\n\t${value}\n}\n")
  }
}

object StringElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new StringElement(config)))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}
