package com.twilightfair.juno.parse.elements.configs

import com.twilightfair.juno.parse.{Parser, Element}
import com.twilightfair.juno.lexx.{Lexx, Token}
import scala.collection.mutable

/**
 * Created by jthomas on 2/10/14.
 */
class InfixConfig(
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val left: Element,
  val right: Element
) extends ElementConfig(lexx, None, data)
