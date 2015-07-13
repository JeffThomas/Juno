package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.asoft.elements.traits.AssignsVariables
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Value, Process}

import scala.collection.mutable

/**
 * Created by jthomas on 12/17/13.
 */
class ReadElement(config: PrefixConfig)
  extends Element(config) with AssignsVariables {

  override var next: Option[Element] = None

  val arrays: Element = config.right
  override def getValue: Any = arrays.getValue

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    val readsEl = proc.heap.headOption
    var index = proc.data("ReadIndex").asInstanceOf[Int]
    readsEl.map { reads =>
      // this could be a single value or a list of values
      // if it's a single we turn it into a list of one so
      // we can treat them both the same
      val readsList = reads.value match {
        case rl: List[Any] => rl
        case _ => List(reads)
      }
      readsList.map { readInto =>
        // we have the var we're going to read into, now we need the value to put into it
        val readValue = Some(proc.data("Data").asInstanceOf[List[Value]](index))
        assignVariable(Some(readInto.asInstanceOf[Value]), readValue, proc.data("Namespace").asInstanceOf[mutable.HashMap[String, Value]])
        index += 1 // How old-school!
      }
    }
    // thankfully AppleSoft does not multi thread
    proc.data("ReadIndex") = index
    setNextElementRun(proc, proc.heap.tailSafe)
  }

  override def run(proc: Process): Process ={
    proc.copy(runStack = arrays.run _ :: execute _ :: proc.runStack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    arrays.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"Read${ReadElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val a = arrays.graph(graph)
    val buf = new StringBuilder
    buf ++= a._2 + s"class ${graphName}\n"
    buf ++= s"${graphName} <|-- ${a._1}: arrays\n"
    (graphName, buf.toString())
  }
}

object ReadElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new ReadElement(config.asInstanceOf[PrefixConfig])
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}