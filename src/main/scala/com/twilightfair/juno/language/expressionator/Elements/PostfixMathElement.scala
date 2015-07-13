package com.twilightfair.juno.language.expressionator.elements

import com.twilightfair.juno.parse.elements.PostfixElement
import com.twilightfair.juno.parse.elements.configs.{PostfixConfig, ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.runtime.{Value, Process}

/**
 * Created by jthomas on 7/2/14.
 */
class PostfixMathElement(config: PostfixConfig)
  extends PostfixElement(config){

  override var next: Option[Element] = None

  val operator: String = config.lexx.currentToken.get.value
  val left: Element = config.left

  def math(left: Float): Float = {
    operator match {
      case "++" =>
        left + 1
      case "--" =>
        left - 1
      case _ =>
        0.0f
    }
  }

  def execute(proc: Process): Process = {
    val left = proc.heap.headOption
    val ret = left.map { left1 =>
      left1.value match {
        case li: Int =>
          math(li.toFloat).toInt
        case lf: Float =>
          math(lf)
        case _ =>
          0
      }
    }
    proc.copy(heap = Value(ret.get, None) :: proc.heap.tail)
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = left.run _ :: execute _ :: setNext(proc.runStack))
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: $operator\n"
    left.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"PostFixMath${PostfixMathElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val l = left.graph(graph)
    val buf = new StringBuilder
    val op = "\"" + operator + "\""
    buf ++= l._2 + s"class $graphName{\n\t$op\n}\n"
    buf ++= s"$graphName <|-- ${l._1}: left\n"
    (graphName, buf.toString())
  }
}

object PostfixMathElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new PostfixMathElement(config.asInstanceOf[PostfixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}
