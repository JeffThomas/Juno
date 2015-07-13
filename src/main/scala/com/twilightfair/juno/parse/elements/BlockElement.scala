package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.parse.{Out, ElementFactory, Element}
import com.twilightfair.juno.parse.elements.configs.{BlockConfig, ElementConfig}
import com.twilightfair.juno.runtime.Process
import com.twilightfair.juno.language.expressionator.elements.FloatElement

/**
 * Created by jthomas on 12/17/13.
 */
class BlockElement(config: BlockConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  var block: Out = config.block
  var token: String = config.lexx.currentToken.get.value

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def getValue: Any = block

  override def run(proc: Process): Process = {
    if (!block.elements.isEmpty){
      proc.copy(runStack = block.elements(0).run _ :: proc.runStack)
    } else {
      proc.copy(runStack = proc.runStack.tail)
    }
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $token\n"
    next.foldLeft(indent) { (i, n) =>
      val ind = i + "  "
      n.tree(ind, buffer)
      ind
    }
    buffer
  }

  override lazy val graphName = s"Block${FloatElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val buf = new StringBuilder
    buf ++= graph
    buf ++= s"package ${graphName}{\n"
    buf ++= block.foldLeft("")((buf, e) => e.graph(buf)._2)
    buf ++= "}\n"
    (graphName, buf.toString())
  }
}

object BlockElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new BlockElement(config.asInstanceOf[BlockConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}