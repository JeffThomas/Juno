package com.twilightfair.juno.language.asoft.elements.configs

import com.twilightfair.juno.parse.Element
import com.twilightfair.juno.lexx.Lexx
import scala.collection.mutable
import com.twilightfair.juno.parse.elements.configs.ElementConfig

/**
 * Created by jthomas on 2/10/14.
 */
class InfixConcatConfig(
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val left: Element,
  val right: Option[Element]
) extends ElementConfig(lexx, None, data)
