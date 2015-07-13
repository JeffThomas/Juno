package com.twilightfair.juno.parse.elements

import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.parse.{ElementFactory, Element}
import com.twilightfair.juno.parse.elements.configs.{InfixConfig, ElementConfig}
import com.twilightfair.juno.runtime.{Process, Value}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/12/13.
 */

class NopElement(config: ElementConfig)
  extends Element(config){

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )

  override var next: Option[Element] = _

  override def getInfo = info

  lazy val graphName = ???
  override def graph(graph: String): (String, String) = ???

  override def run(proc: Process): Process = ???

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = ???

  override def getValue: Any = ???
}

object NopElement extends ElementFactory {

  def emptyNopElement = new NopElement(new ElementConfig(Lexx.emptyLexx, None, HashMap[String, Any]()))

  def apply(config: ElementConfig) = Right(Some(new NopElement(config.asInstanceOf[InfixConfig])))

  def apply() = Right(Some(new ElementConfig(Lexx.emptyLexx, None, HashMap[String, Any]())))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}