package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.language.expressionator.elements.FloatElement
import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class LabelElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val value: String = config.lexx.currentToken.get.value
  override def getValue: Any = value

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process = {
    //proc.data("Labels").asInstanceOf[ParHashMap[String, Element]](value) = this
    setNextElementRun(proc)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $value\n"
  }

  lazy val graphName = s"Label${LabelElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val buf = new StringBuilder
    buf ++= graph
    buf ++= s"class $graphName{\n\t${value}\n}\n"
    buf ++= graphLinkNext("line")
    (graphName, buf.toString())
  }
}

object LabelElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new LabelElement(config)
    (config.data("Labels").asInstanceOf[HashMap[String, Element]])(element.value) = element
    config.data("If").asInstanceOf[List[IfElement]].map { ifEl =>
      ifEl.skipTo = Some(element)
    }
    config.data("If") = Nil
    config.data("OnGosub").asInstanceOf[Option[OnGosubElement]].map { goSub =>
      goSub.returnToOpt = Some(element)
      config.data("OnGosub") = None
    }
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}