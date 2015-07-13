package com.twilightfair.juno

/**
 * Created by jthomas on 6/19/14.
 */
package object Constants {
  final val PRECEDENCE_NONE        = 0
  final val PRECEDENCE_ASSIGNMENT  = 1
  final var PRECEDENCE_CONDITIONAL = 2
  final val PRECEDENCE_SUM         = 3
  final val PRECEDENCE_PRODUCT     = 4
  final val PRECEDENCE_EXPONENT    = 5
  final val PRECEDENCE_PREFIX      = 6
  final val PRECEDENCE_POSTFIX     = 7
  final val PRECEDENCE_CALL        = 8
  final val PRECEDENCE_END         = 100
}