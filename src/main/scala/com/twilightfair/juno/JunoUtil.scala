package com.twilightfair.juno

import com.twilightfair.juno.runtime.{Value, Process}
import java.lang.Process

/**
 * Created by jthomas on 7/23/14.
 */
object JunoUtil {
  implicit class ListValueSafeTail[T](l:List[T]) {
    def tailSafe: List[T] = {
      if (l.isEmpty)
        Nil
      else
        l.tail
    }
  }
}
