package com.twilightfair.juno.language.asoft.elements

import com.twilightfair.juno.parse.{Element, ElementFactory}
import com.twilightfair.juno.parse.elements.configs.{OptionPrefixConfig, PrefixConfig, ElementConfig}
import com.twilightfair.juno.runtime
import com.twilightfair.juno.runtime.{Value, Process}

import scala.collection.mutable.HashMap
import com.twilightfair.juno.JunoUtil.ListValueSafeTail

/**
 * Created by jthomas on 12/17/13.
 */
class IdentifierElement(config: OptionPrefixConfig)
  extends Element(config) {

  def default(typ: Char) = typ match {
    case '$' =>
      ""
    case '%' =>
      0
    case _ =>
      0.0
  }

  override var next: Option[Element] = None

  val sub = config.right

  private def setId(ident: String): String = {
    val v: String = if (ident.length > 2) ident.substring(0, 2) else ident
    if (ident.length > 2 && (ident.last == '$' || ident.last == '%')) v + ident.last else v
  }
  val value: String = setId(config.lexx.currentToken.get.value)
  val typ: Char = if (value.last == '$' || value.last == '%') value.last else ' '

  override def tree(indent: String, buffer: StringBuilder): StringBuilder = {
    buffer ++= s"$indent$graphName : $value\n"
  }

  lazy val graphName = s"Identifier${IdentifierElement.getCount}"
  override def graph(graph: String): (String, String) = {
    (graphName, graph + s"class $graphName{\n\t$value\n}\n")
  }

  override def getValue: Any = value

  def execute(proc: Process): Process = {
    val nameSpace:HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
    val subIndex = proc.heap.headOption
    if (proc.data("Dim").asInstanceOf[Boolean]) {
      // we're defining the array, subIndex would be the size limit but we ignore it
      val v = Value(HashMap[Int ,Any](), Some(this))
      nameSpace(value) = v
      setNextElementRun(proc, v :: proc.heap.tailSafe)
    } else {
      case class NodeLevel(
        map: HashMap[Int ,Any],
        count: Int,
        v: Option[Any]
      )
      // we're accessing the array
      nameSpace.get(value).map { arr =>
        if (subIndex.isDefined && subIndex.get.value.isInstanceOf[List[Any]]){
          // multiple array
          val list = subIndex.get.value.asInstanceOf[List[Value]]
          val currentMap = arr.value.asInstanceOf[HashMap[Int, Any]]
          val res = list.foldLeft(NodeLevel(currentMap, 0, None)) { (r, c) =>
            val v = r.map.getOrElse(c.value.asInstanceOf[Int], None)
            r.count match {
              case i if r.count == list.length - 1 =>
                v match {
                  case None =>
                    NodeLevel(r.map, r.count, Some(default(typ)))
                  case value =>
                    NodeLevel(r.map, r.count, Some(value))
                }
              case _ =>
                v match {
                  case None =>
                    val newMap = HashMap[Int, Any]()
                    r.map(c.value.asInstanceOf[Int]) = newMap
                    NodeLevel(newMap, r.count + 1, None)
                  case map =>
                    NodeLevel(map.asInstanceOf[HashMap[Int, Any]], r.count + 1, None)
                }
            }
          }
          setNextElementRun(proc, Value(res.v.getOrElse(0), Some(this), Some(Right(subIndex.get.value.asInstanceOf[List[Value]]))) :: proc.heap.tailSafe)
        } else {
          val i = subIndex.get.value match {
            case ri: Int =>
              ri
            case rf: Float =>
              rf.toInt
            case rs: String =>
              rs.toInt
          }
          val v = arr.value.asInstanceOf[HashMap[Int, Any]].getOrElse(i, default(typ))
          setNextElementRun(proc, Value(v, Some(this), Some(Left(i))) :: proc.heap.tailSafe)
        }
      }.getOrElse{
        setNextElementRun(proc)
      }
    }
  }

  override def run(proc: runtime.Process): runtime.Process = {
    sub.map { s =>
      // we're an array
      proc.copy(runStack = s.run _ :: execute _ :: proc.runStack)
    }.getOrElse {
      val nameSpace: HashMap[String, Value] = proc.data("Namespace").asInstanceOf[HashMap[String, Value]]
      // side effects ahoy!
      val v = nameSpace.getOrElse(value, typ match {
        case '$' =>
          Value("", Some(this))
        case '%' =>
          Value(0, Some(this))
        case _ =>
          Value(0.0, Some(this))
      })
      nameSpace(value) = v
      setNextElementRun(proc, v :: proc.heap)
    }
  }
}

object IdentifierElement extends ElementFactory {
  def apply(config: ElementConfig) = Right(Some(new IdentifierElement(config.asInstanceOf[OptionPrefixConfig])))

  var count: Integer = 0
  def getCount = {
    count += 1
    count
  }
}
