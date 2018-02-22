package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Bishop(placeHorizontal: Int, placeVertical: Int) extends ChessShape{

  override def brokenField(horizontal: Int, vertical: Int): Boolean = {
    abs(placeHorizontal - horizontal) == abs(placeVertical - vertical)
  }
}

object Bishop {
  def apply(placeHorizontal: Int, placeVertical: Int): Bishop = new Bishop(placeHorizontal, placeVertical)
}