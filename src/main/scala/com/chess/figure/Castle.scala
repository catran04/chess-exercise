package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Castle extends ChessShape {

  override val priority = 2

  override def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean = {
    val coincidenceRaw: Boolean = horizontal == otherHorizontal
    val coincidenceColumn: Boolean = vertical == otherVertical
    coincidenceRaw || coincidenceColumn
  }
}

object Castle {
  def apply(): Castle = new Castle
}