package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Castle(placeHorizontal: Int, placeVertical: Int) extends ChessShape {

  override def brokenField(horizontal: Int, vertical: Int): Boolean = {
    val coincidenceRaw: Boolean = placeHorizontal == horizontal
    val coincidenceColumn: Boolean = placeVertical == vertical
    coincidenceRaw || coincidenceColumn
  }
}

object Castle {
  def apply(): Castle = new Castle()
}