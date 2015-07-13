package com.twilightfair.juno.language.asoft.elements.traits

import com.twilightfair.juno.language.asoft.elements.IdentifierElement
import com.twilightfair.juno.runtime.Value

import scala.collection.mutable.HashMap

/**
 * Created by Jeff on 6/11/2015.
 */
trait AssignsVariables {
  def assignVariable(leftOpt: Option[Value], rightOpt: Option[Value], nameSpace: HashMap[String, Value]): HashMap[String, Value] = {
    leftOpt.map{ left =>
      left.ref.map{ ref =>
        rightOpt.map{ right =>
          val v = right.value match {
            case ri: Int =>
              ref.asInstanceOf[IdentifierElement].typ match {
                case '$' =>
                  ri.toString
                case '%' =>
                  ri
                case ' ' =>
                  ri.toFloat
              }
            case rf: Float =>
              ref.asInstanceOf[IdentifierElement].typ match {
                case '$' =>
                  rf.toString
                case '%' =>
                  rf.toInt
                case ' ' =>
                  rf
              }
            case rs: String =>
              ref.asInstanceOf[IdentifierElement].typ match {
                case '$' =>
                  rs
                case '%' =>
                  rs.trim.toInt
                case ' ' =>
                  rs.trim.toFloat
              }
          }
          left.index match {
            case Some(i) =>
              val oldMap = nameSpace(ref.getValue.toString).value.asInstanceOf[HashMap[Int ,Any]]
              i match {
                case Left(index) =>
                  nameSpace(ref.getValue.toString) = Value(oldMap + (index -> v))
                case Right(arrayIndex) => {
                  case class NodeLevel(
                                        map: HashMap[Int ,Any],
                                        level: Int
                                        )
                  val size = arrayIndex.size
                  arrayIndex.foldLeft(NodeLevel(oldMap, size))((nl, index) =>
                    if (nl.level == 1) {
                      nl.map(index.value.asInstanceOf[Int]) = v
                      NodeLevel(oldMap, nl.level - 1)
                    } else {
                      nl.map.get(index.value.asInstanceOf[Int]) match {
                        case None =>
                          val newMap: HashMap[Int, Any] = HashMap()
                          nl.map(index.value.asInstanceOf[Int]) = newMap
                          NodeLevel(newMap, nl.level - 1)
                        case Some(thisMap) =>
                          NodeLevel(thisMap.asInstanceOf[HashMap[Int, Any]], nl.level - 1)
                      }
                    }
                  )
                }
              }
            case None =>
              nameSpace(ref.getValue.toString) = Value(v, Some(ref))
          }
        }
      }
    }
    nameSpace
  }

}
