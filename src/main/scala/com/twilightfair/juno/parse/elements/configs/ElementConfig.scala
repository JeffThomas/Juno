package com.twilightfair.juno.parse.elements.configs

import com.twilightfair.juno.lexx.{Lexx, Token}
import scala.collection.mutable
import com.twilightfair.juno.parse.Element

/**
 * Created by jthomas on 2/10/14.
 */
class ElementConfig(val lexx: Lexx, val next: Option[Element], val data: mutable.HashMap[String, Any]){
}
