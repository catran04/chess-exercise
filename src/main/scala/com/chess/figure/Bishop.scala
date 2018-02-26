package com.chess.figure

import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Bishop extends ChessShape{

  override val priority = 3

  override def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean = {
    abs(horizontal - otherHorizontal) == abs(vertical - otherVertical)
  }
}

object Bishop {
  def apply(): Bishop = new Bishop()
}