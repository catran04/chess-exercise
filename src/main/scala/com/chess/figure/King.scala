package com.chess.figure

import scala.math.abs
/**
  * Created by Administrator on 2/20/2018.
  */
class King() extends ChessShape{

  override val priority = 5

  override def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean = {
    abs(horizontal - otherHorizontal) <= 1 && abs(vertical - otherVertical) <= 1
  }

}

object King {
  def apply(): King = new King
}
