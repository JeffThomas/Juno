package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.expressionator.elements.FloatElement
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.runtime.{Process, Value}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class ForElement(config: PrefixConfig)
  extends Element(config) {

  override var next: Option[Element] = None

  var continueOpt: Option[NextElement] = None

  // Extract the TO element for the range and the name of the Iterator variable
  val (range: InfixToElement, iterator: Option[Any]) = config.right match {
    case ass: InfixAssignmentElement =>
      ass.right match {
        case to: InfixToElement => {
          val ident = ass.left match {
            case ident: IdentifierElement =>
              // we have a TO and an iterator variable, tell the TO what the iterator var is
              to.iteratorOpt = Some(ident)
              Some(ident)
            case _ =>
              println(s"No identifier for iterator in FOR at line ${config.lexx.lineCount}")
              "NONE"
          }
          (to, ident)
        }
        case _ =>
          println(s"No range for FOR at line ${config.lexx.lineCount}")
          (ass, None)
      }
    case _ => {
      println(s"No iterator value for FOR at line ${config.lexx.lineCount}")
      (config.right, None)
    }
  }

  override def getValue: Any = iterator
  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    val nameSpace: HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
    val indexValue = nameSpace(iterator.get.asInstanceOf[IdentifierElement].value)
    val rightOpt = proc.heap.headOption
    val leftOpt = proc.heap.tailSafe.headOption

    leftOpt.flatMap{ left =>
      rightOpt.flatMap{ right =>
        val leftf = toFloat(left.value)
        val rightf = toFloat(right.value)
        val index = toFloat(indexValue.value)
        val min = if (leftf > rightf) rightf else leftf
        val max = if (leftf > rightf) leftf else rightf
        index match {
          case i if i > max || i < min => {
            continueOpt.map { continue =>
              Some(proc(proc.heap.tailSafe.tailSafe, continue.terminate(iterator.get.asInstanceOf[IdentifierElement].value) _))
            }.getOrElse{
              println(s"Nowhere to go after finishing FOR loop at line ${info("row")}")
              Some(proc(proc.heap.tailSafe.tailSafe))
            }
          }
          case _ => {
            range.stepOpt flatMap { s =>
              s.next map { n =>
                Some(proc.copy(runStack = n.run _ :: proc.runStack, heap = proc.heap.tailSafe.tailSafe))
              }
            }
          }.getOrElse(
              Some(proc.copy(runStack = setNext(proc.runStack), heap = proc.heap.tailSafe.tailSafe))
            )
        }
      }
    }.getOrElse{
      println(s"Could not get min or max for FOR statement at line ${info("row")}")
      proc(proc.heap.tailSafe.tailSafe)
    }
  }

  def loop(proc: Process): Process = {
    proc.copy(runStack = range.run _ :: execute _ :: proc.runStack)
  }

  override def run(proc: Process): Process = {
    proc.copy(runStack = range.init _ :: execute _ :: proc.runStack)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName: ${iterator.get.asInstanceOf[IdentifierElement].value.toString}\n"
    range.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"For${ForElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = range.graph(graph)
    val buf = new StringBuilder
    buf ++= r._2 + s"class $graphName{\n\t${iterator.get.asInstanceOf[IdentifierElement].value}\n}\n"
    buf ++= s"$graphName <|-- ${r._1}: range\n"
    (graphName, buf.toString())
  }
}

object ForElement extends ElementFactory {
  def apply(config: ElementConfig) = {
    val element = new ForElement(config.asInstanceOf[PrefixConfig])
    config.data("For").asInstanceOf[HashMap[String, ForElement]](element.iterator.get.asInstanceOf[IdentifierElement].value) = element
    config.data("Step") = Some(element)
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}