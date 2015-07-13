package com.twilightfair.juno.parse.elements.configs

import com.twilightfair.juno.lexx.Lexx
import scala.collection.mutable
import com.twilightfair.juno.parse.Element

/**
 * Created by jthomas on 7/1/14.
 */
class PrefixConfig (
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val right: Element
) extends ElementConfig(lexx, None, data)
