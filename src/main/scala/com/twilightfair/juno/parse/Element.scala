package com.twilightfair.juno.parse

import com.twilightfair.juno.parse.elements.configs.ElementConfig
import com.twilightfair.juno.runtime.{Value, NextRun, Process}

/**
 * Created by jthomas on 12/12/13.
 */

abstract class Element(config: ElementConfig)  {
  def getValue: Any
  def getInfo: Map[String, Any] = ???

  var next: Option[Element]
  def getNext: Option[Element] = next

  def run(proc: Process): Process

  val graphName: String
  def graphLinkNext(l: String) = next.map { n => s"${graphName} <|-- ${n.graphName}: $l\n" }.getOrElse("")
  def graph(graph: String): (String, String)

  def tree(indent: String, buffer: StringBuilder): StringBuilder

  override def toString: String = "Element"

  def setNext(runStack: List[(Process) => Process]) =
    next.map {
      nextElement => nextElement.run _ :: runStack
    }.getOrElse(runStack)

  def setNextElementRun(proc: Process): Process = {
    next.map { nextElement =>
      proc(nextElement.run _)
    }.getOrElse(proc)
  }
  
  def setNextElementRun(proc: Process, heap: List[Value]): Process = {
    next.map { nextElement =>
      proc(heap, nextElement.run _)
    }.getOrElse(proc.copy(heap = heap))
  }

  def toFloat(v: Any) = {
    try {
      v match {
        case i: Int => i.toFloat
        case f: Float => f
        case s: String => s.toFloat
        case _ => 0f
      }
    }
    catch {
      case e: NumberFormatException => 0f
    }
  }
}

object Element{
}

abstract class ElementFactory extends (ElementConfig => Either[ParserError, Option[Element]])
