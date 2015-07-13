package com.twilightfair.juno.language.asoft.elements.configs

import com.twilightfair.juno.language.asoft.elements.ForElement
import com.twilightfair.juno.lexx.Lexx
import com.twilightfair.juno.parse.Element
import com.twilightfair.juno.parse.elements.configs.ElementConfig

import scala.collection.mutable

/**
 * Created by jthomas on 7/1/14.
 */
class NextConfig (
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val right: Element,
  var inermostFor: ForElement
) extends ElementConfig(lexx, None, data)
