package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.JunoUtil.ListValueSafeTail
import com.twilightfair.juno.language.asoft.elements.configs.NextConfig
import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.parse.elements.configs.{ElementConfig, PrefixConfig}
import com.twilightfair.juno.parse.{ParserError, Element, ElementFactory}
import com.twilightfair.juno.runtime.{Value, Process}

import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 12/17/13.
 */
class NextElement(config: NextConfig, forMap: Map[String, ForElement])
  extends Element(config) {

  override var next: Option[Element] = None

  val iters: Element = config.right
  var fors: Map[String, ForElement] = forMap.toMap
  var inermostFor: ForElement = config.inermostFor
  override def getValue: Any = iters

  val info: Map[String, Any] = Map(
    "row" -> config.lexx.lineCount,
    "col" -> (config.lexx.index - config.lexx.lineStart)
  )
  override def getInfo = info

  def execute(proc: Process): Process = {
    setNextElementRun(proc, proc.heap.tailSafe)
  }

  def terminate2(iterator: String)(proc: Process): Process = {
    val iterators = proc.heap.headOption
    (iterators flatMap { v =>
      v.ref match {
        case Some(value) if value.isInstanceOf[IdentifierElement] =>
          Some(setNextElementRun(proc))
        case _ => {
          val l = iterators.get.value.asInstanceOf[List[Value]]
          val nextIterator = l.foldLeft[Either[Boolean, String]](Left(false))((found, i) => {
            found match {
              case Left(f) if f =>
                Right(i.ref.get.getValue.asInstanceOf[String])
              case Left(f) if !f =>
                if (i.ref.get.getValue.asInstanceOf[String] == iterator)
                  Left(true)
                else
                  Left(false)
              case Right(f) =>
                Right(f)
            }
          })
          nextIterator match {
            case Left(f) =>
              // continuing on
              Some(setNextElementRun(proc))
            case Right(i) =>
              fors.get(i).map { nextFor =>
                Some(proc(nextFor.loop _))
              }.getOrElse {
                println("No FORs to finish with")
                None
              }
            case _ =>
              None
          }
        }
      }
    }).getOrElse(proc.copy())
  }

  def terminate(i: String)(proc: Process): Process = {
    proc(iters.run _, terminate2(i) _)
  }

  override def run(proc: Process): Process = {
    proc(inermostFor.loop _)
  }

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName\n"
    iters.tree(indent+"  ", buffer)
  }

  lazy val graphName = s"Next${NextElement.getCount}"
  override def graph(graph: String): (String, String) = {
    val r = iters.graph(graph)
    val buf = new StringBuilder
    buf ++= r._2 + s"class ${graphName}{\n\t${r._1}\n}\n"
    buf ++= s"${graphName} <|-- ${r._1}: for(s)\n"
    (graphName, buf.toString())
  }
}

object NextElement extends ElementFactory {
  def doNextCommas(
      element: NextElement,
      forMap: HashMap[String, ForElement],
      comma: CommaConcatElement)
      : Unit = {
    forMap(comma.left.asInstanceOf[IdentifierElement].value).continueOpt = Some(element)
    comma.right match {
      case c: CommaConcatElement =>
        doNextCommas(element, forMap, c)
      case i: IdentifierElement => {
        val f: ForElement = forMap(i.value)
        f.continueOpt = Some(element)
      }
    }
  }

  def apply(config: ElementConfig) = {
    val forMap = config.data("For").asInstanceOf[HashMap[String, ForElement]]
    val element = new NextElement(config.asInstanceOf[NextConfig], forMap.toMap)
    element.iters match {
      case c: CommaConcatElement => {
        doNextCommas(element, forMap, c)
      }
      case i: IdentifierElement => {
        val f: ForElement = forMap(i.value)
        f.continueOpt = Some(element)
      }
    }
    Right(Some(element))
  }

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}