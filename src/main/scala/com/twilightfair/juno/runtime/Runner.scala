package com.twilightfair.juno.runtime

import com.twilightfair.juno.language.Language
import com.twilightfair.juno.parse.{Parser, Out}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.collection.mutable.HashMap

/**
 * Created by jthomas on 4/14/15.
 */
trait Runner {

  def parse(script: Stream[Char]): Out
  val lang: Language

  def buildProcess(out: Out): Process = {
    Process(lang, Nil, Nil, HashMap[String, Any](), Nil)
  }

  def runExpression(proc: Process): Future[Process] = {
    if (proc.runStack.headOption.isEmpty)
      Future.successful(proc)
    else {
      runExpression(proc.runStack.headOption.get(proc.copy(runStack = proc.runStack.tail)))
    }
  }

  def run(out: Out, process: Process): Future[Process] = {
    if (!out.elements.isEmpty) {
      runExpression(process.copy(runStack = out.elements(0).run _ :: process.runStack))
    } else
      Future.successful(process)
  }

  def run(script: Stream[Char]): Future[Process] = {
    val out: Out = parse(script)
    if (out.error.isDefined && !out.error.get.state.startsWith("Done:")) {
      Future.successful(Process(lang, Nil, Nil, HashMap[String, Any](), Nil, List(out.error.get)))
    } else {
      val process: Process = buildProcess(out)
      run(out, process)
    }
  }
}

