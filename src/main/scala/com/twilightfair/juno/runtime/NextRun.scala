package com.twilightfair.juno.runtime

/**
 * Created by jthomas on 6/24/14.
 */
case class NextRun(env: Process, next: Option[(Process) => NextRun])
