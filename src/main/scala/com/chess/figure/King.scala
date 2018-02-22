package com.chess.figure

import scala.math.abs
/**
  * Created by Administrator on 2/20/2018.
  */
class King(placeHorizontal: Int, placeVertical: Int) extends ChessShape{

  override def brokenField(horizontal: Int, vertical: Int): Boolean = {
    abs(placeHorizontal - horizontal) <= 1 && abs(placeVertical - vertical) <= 1
  }

}

object King {
  def apply(placeHorizontal: Int, placeVertical: Int): King = new King(placeHorizontal, placeVertical)
}
