package com.twilightfair.juno.runtime

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.parse.ParserError

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by Jeff on 6/16/2014.
 */
trait JunoProcess{
  val lang: Language
  val runStack: List[(Process) => Process]
  val heap: List[Value]
  val data: HashMap[String, Any]
}

case class Process(lang: Language, runStack: List[(Process) => Process], heap: List[Value], data: mutable.HashMap[String, Any], outBuffer: List[String], errors: List[ParserError] = Nil) extends JunoProcess {

  def apply(stack: ((Process) => Process)*): Process = {
    copy(runStack = stack.toList ::: this.runStack)
  }

  def apply(heapNew: List[Value], stack: ((Process) => Process)*): Process = {
    copy(runStack = stack.toList ::: this.runStack, heap = heapNew)
  }

}
