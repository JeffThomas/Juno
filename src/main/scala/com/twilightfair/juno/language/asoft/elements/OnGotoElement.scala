package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.parse.elements.InfixElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, InfixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class OnGotoElement(config: InfixConfig)
  extends InfixElement(config) {

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left
  val right:Element = config.right

  override def getValue: Any = operator

  def execute(proc: Process): Process = {
    val rightEl = proc.heap.headOption
    val leftEl = proc.heap.tailSafe.headOption
    rightEl.flatMap { right1 =>
      // this could be a single value or a list of values
      // if it's a single we turn it into a list of one so
      // we can treat them both the same
      val r = right1.value match {
        case rl: List[Any] => rl
        case _ => List(right1.value)
      }
      leftEl.flatMap { left1 =>
        val index = left1.value match {
          case i: Int => i - 1
          case fl: Float => fl.toInt - 1
          case _ => 0
        }
        if (index < r.length) {
          val label = r(index).asInstanceOf[Value].value.toString
          val targetOpt: Option[Element] = proc.data("Labels").asInstanceOf[HashMap[String, Element]].get(label)
          targetOpt.map{ target2 =>
            Some(proc.copy(runStack = List(targetOpt.get.run _), heap = Nil))
          }.getOrElse {
            println(s"Label not found to ON GOTO at line ${info("row")}: ${label}")
            None
          }
        } else {
          println(s"Index out of bounds ON GOTO at line ${info("row")}: ${index.toString}")
          None
        }
      }
    }.getOrElse {
      println(s"Failed ON GOTO at line ${info("row")}")
      proc.copy(runStack = proc.runStack.tail, heap = proc.heap.tailSafe)
    }
  }

  override def run(procCurrent: Process): Process = {
    val stackNext = execute _ :: setNext(procCurrent.runStack)
    val stack = left.run _ :: right.run _ :: stackNext
    procCurrent.copy(runStack = stack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    left.tree(indent+"  ", buffer)
    right.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"OnGoto${OnGotoElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val r = right.graph(l._2)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= r._2 + s"class ${graphName}{\n\t${op}\n}\n"
    buf ++= s"${graphName} <|-- ${l._1}: left\n"
    buf ++= s"${graphName} <|-- ${r._1}: right\n"
    (graphName, buf.toString())
  }
}

object OnGotoElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new OnGotoElement(config.asInstanceOf[InfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}