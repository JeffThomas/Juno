package com.twilightfair.juno.parse

/**
 * Created by jthomas on 6/17/14.
 */
case class Out(elements: List[Element], error: Option[ParserError], parser: Parser) {

  @inline final def foldLeft[B](ifEmpty: => B)(f: (B, Element) => B): B = {
    elements.foldLeft(ifEmpty)(f)
  }

}
