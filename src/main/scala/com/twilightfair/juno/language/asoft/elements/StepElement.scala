package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.expressionator.elements.FloatElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{ParserError, Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}

/**
 * Created by jthomas on 12/17/13.
 */
class StepElement(config: PrefixConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val amount: Element = config.right
  override def getValue: Any = amount.getValue

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process ={
    proc.copy(runStack = amount.run _ :: proc.runStack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    amount.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"Step${StepElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val a = amount.graph(graph)
    val buf = new StringBuilder
    buf ++= a._2 + s"class $graphName\n"
    buf ++= s"$graphName <|-- ${a._1}: amount\n"
    (graphName, buf.toString())
  }
}

object StepElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val step = config.data("Step")
    step.asInstanceOf[Some[ForElement]].map { f =>
      config.data("Step") = null
      val element = new StepElement(config.asInstanceOf[PrefixConfig])
      f.range match {
        case r: InfixToElement => r.stepOpt = Some(element)
      }
      // skip the Step in the main execution
      Right(Some(element))
    }.getOrElse(Left(ParserError(s"Step with no For at line ${config.data("row")}", config.lexx, None)))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}