package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, InfixConfig}
import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.runtime.{Process, Value}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class InfixToElement(config: InfixConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val left: Element = config.left
  val right: Element = config.right
  var stepOpt: Option[Element] = None
  var iteratorOpt: Option[IdentifierElement] = None

  override def getValue: Any = iteratorOpt.get.value

  def execute(proc: Process): Process = {
    val nameSpace: HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
    val rightOpt = proc.heap.headOption
    val leftOpt = proc.heap.tailSafe.headOption
    val stepOptMaybe = proc.heap.tailSafe.tailSafe.headOption

    val stepO = stepOptMaybe.flatMap { _.ref flatMap {
      case s: InfixToElement => stepOptMaybe
      case _ => None
    }}

    val newHeap = stepO.map { step =>
      // remove the step element from the heap if needed
      rightOpt.get :: leftOpt.get :: proc.heap.tailSafe.tailSafe.tailSafe
    }.getOrElse(proc.heap)

    leftOpt.map{ left =>
      rightOpt.map{ right =>
        val indexValue = nameSpace(iteratorOpt.get.value)
        val index = toFloat(indexValue.value)
        val step = stepO.map(i => toFloat(i.value)).getOrElse(1f)
        nameSpace(iteratorOpt.get.value) = Value(index + step, iteratorOpt)
      }
    }
    proc.copy(heap = newHeap)
  }

  def stepStep(proc: Process): Process = {
    proc.heap.headOption.map{ stepVal =>
      proc.copy(runStack = left.run _ ::  right.run _ :: execute _ :: setNext(proc.runStack),
        heap = Value(stepVal.value, Some(this)) :: proc.heap.tail)
    }.getOrElse(
        proc.copy(runStack = left.run _ ::  right.run _ :: execute _ :: setNext(proc.runStack))
      )
  }

  override def run(procCurrent: Process): Process = {
    stepOpt match {
      case None =>
        val stack = left.run _ ::  right.run _ :: execute _ :: setNext(procCurrent.runStack)
        procCurrent.copy(runStack = stack)
      case Some(step) =>
        procCurrent.copy(runStack = step.run _ :: stepStep _ :: procCurrent.runStack)
    }
  }

  def init2(proc: Process): Process = {
    val nameSpace: HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
    val leftOpt = proc.heap.tailSafe.headOption
    leftOpt.map { left =>
      nameSpace(iteratorOpt.get.value) = Value(left.value, iteratorOpt)
    }
    // we leave the left and right in the heap for the FOR
    proc.copy()
  }

  def init(proc: Process): Process = {
    // set the iterator to the min level
    val stack = left.run _ :: right.run _ :: init2 _ :: setNext(proc.runStack)
    proc.copy(runStack = stack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : Step $stepOpt : Iterator $iteratorOpt\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"InfixTo${InfixToElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val r = right.graph(l._2)
    val s = stepOpt.map{
      _.graph(r._2)
    }.getOrElse(r)
    val iter = "\"" + iteratorOpt.get.value + "\""
    val buf = new StringBuilder
    buf ++= s._2 + s"class ${graphName}{\n\t${iter}\n}\n"
    buf ++= s"${graphName} <|-- ${l._1}: min\n"
    buf ++= s"${graphName} <|-- ${r._1}: max\n"
    stepOpt.map { step =>
      buf ++= s"${graphName} <|-- ${s._1}: step\n"
      step
    }
    (graphName, buf.toString())
  }
}

object InfixToElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new InfixToElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}