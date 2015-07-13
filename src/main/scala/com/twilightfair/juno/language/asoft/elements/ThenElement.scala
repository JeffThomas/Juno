package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.language.expressionator.elements.{IntegerElement, FloatElement}
import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.Process

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class ThenElement(config: ElementConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  override def getValue: Any = "THEN"

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  override def run(proc: Process): Process ={
    next.flatMap { _ match {
      case n if n.isInstanceOf[IntegerElement] =>
        val targetOpt: Option[Element] = proc.data("Labels").asInstanceOf[HashMap[String, Element]].get(n.getValue.toString)
        targetOpt.map{ target2 =>
          Some(proc.copy(runStack = List(targetOpt.get.run _), heap = Nil))
        }.getOrElse {
          println(s"Label not found to THEN GOTO at line ${info("row")}: ${n.getValue.toString}")
          None
        }
      case _ => None
    }}.getOrElse(
        proc.copy(runStack = setNext(proc.runStack))
      )
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    next.foldLeft(indent){ (i, n) =>
      val ind = i + "  "
      n.tree(ind, buffer)
      ind
    }
    buffer
  }

  lazy val graphName = s"Then${ThenElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val buf = new StringBuilder
    buf ++= graph
    buf ++= s"class ${graphName}{\n}\n"
    buf ++= graphLinkNext("then")
    (graphName, buf.toString())
  }
}

object ThenElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new ThenElement(config)
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}