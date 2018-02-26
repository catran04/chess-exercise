package com.chess.figure

import scala.collection.mutable
import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Queen extends ChessShape {

  override val priority = 1

  override def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean = {
    val bishopBrokes: Boolean = abs(horizontal - otherHorizontal) == abs(vertical - otherVertical)
    val castleBrokes: Boolean = {
      val coincidenceRaw: Boolean = horizontal == otherHorizontal
      val coincidenceColumn: Boolean = vertical == otherVertical
      coincidenceRaw || coincidenceColumn
    }
    bishopBrokes || castleBrokes
  }
}

object Queen {
  def apply(): Queen = new Queen
}