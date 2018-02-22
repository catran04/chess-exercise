package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Knight(placeHorizontal: Int, placeVertical: Int) extends ChessShape{

  override def brokenField(horizontal: Int, vertical: Int): Boolean = {
    val knightBrokes = (abs(placeHorizontal - horizontal), abs(placeVertical - vertical))
    knightBrokes == (1,2) || knightBrokes == (2,1)
  }
}

object Knight {
  def apply(placeHorizontal: Int, placeVertical: Int): Knight = new Knight(placeHorizontal, placeVertical)
}
