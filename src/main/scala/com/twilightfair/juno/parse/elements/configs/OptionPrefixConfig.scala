package com.twilightfair.juno.parse.elements.configs

import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.parse.Element

import scala.collection.mutable

/**
 * Created by jthomas on 7/1/14.
 */
class OptionPrefixConfig (
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val right: Option[Element]
) extends ElementConfig(lexx, None, data)
