package com.chess.figure

import scala.collection.mutable
import scala.math.abs

/**
  * Created by Administrator on 2/20/2018.
  */
class Queen(placeHorizontal: Int, placeVertical: Int) extends ChessShape {


  override def brokenField(horizontal: Int, vertical: Int): Boolean = {
    val bishopBrokes: Boolean = abs(placeHorizontal - horizontal) == abs(placeVertical - vertical)
    val castleBrokes: Boolean = {
      val coincidenceRaw: Boolean = placeHorizontal == horizontal
      val coincidenceColumn: Boolean = placeVertical == vertical
      coincidenceRaw || coincidenceColumn
    }
    bishopBrokes || castleBrokes
  }
}

object Queen {
  def apply(placeHorizontal: Int, placeVertical: Int): Queen = new Queen(placeHorizontal, placeVertical)
}