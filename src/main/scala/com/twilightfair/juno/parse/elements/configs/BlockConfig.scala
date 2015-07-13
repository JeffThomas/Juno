package com.twilightfair.juno.parse.elements.configs

import com.twilightfair.juno.parse.{Out, Element}
import com.twilightfair.juno.lexx.{Lexx, Token}
import scala.collection.mutable

/**
 * Created by jthomas on 5/1/14.
 */
class BlockConfig(
  override val lexx: Lexx,
  override val data: mutable.HashMap[String, Any],
  val block: Out
) extends ElementConfig(lexx, None, data)
