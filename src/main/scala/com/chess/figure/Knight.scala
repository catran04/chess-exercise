package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Knight extends ChessShape{

  override val priority = 4

  override def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean = {
    val knightBrokes = (abs(horizontal - otherHorizontal), abs(vertical - otherVertical))
    knightBrokes == (1,2) || knightBrokes == (2,1) || knightBrokes == (0,0)
  }
}

object Knight {
  def apply(): Knight = new Knight
}
