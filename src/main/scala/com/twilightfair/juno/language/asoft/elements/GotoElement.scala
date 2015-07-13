package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.elements.configs.{PrefixConfig, ElementConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Value, Process}

import com.twilightfair.juno.JunoUtil.ListValueSafeTail

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class GotoElement(config: PrefixConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  val target: Element = config.right
  val value: String = config.lexx.currentToken.get.value
  override def getValue: Any = value

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    val tOption = proc.heap.headOption
    tOption.flatMap { t =>
      if (t.isInstanceOf[Value]) {
        val targetOpt: Option[Element] = proc.data("Labels").asInstanceOf[HashMap[String, Element]].get(t.value.toString)
        targetOpt.map{ target2 =>
          Some(proc.copy(runStack = List(targetOpt.get.run _), heap = Nil))
        }.getOrElse {
          println(s"Label not found to GOTO at line ${info("row")}: ${t.value.toString}")
          None
        }
      } else {
        println(s"Invalid Label to GOTO at line ${info("row")}")
        None
      }
    }.getOrElse{
      println(s"No Label to GOTO at line ${info("row")}")
      proc.copy(runStack = proc.runStack.tail, heap = proc.heap.tailSafe)
    }
  }

  override def run(proc: Process): Process ={
    proc.copy(runStack = target.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $value\n"
  }

  lazy val graphName = s"Goto${GotoElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val cl = s"class $graphName{\n\t$value\n}\n"
    (graphName, graph + cl)
  }
}

object GotoElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new GotoElement(config.asInstanceOf[PrefixConfig])
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}