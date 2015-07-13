package com.twilightfair.juno.runtime

import com.twilightfair.juno.parse.Element

/**
 * Created by jthomas on 6/24/14.
 */
case class Value(value: Any, ref: Option[Element] = None, index: Option[Either[Int, List[Value]]] = None){}
