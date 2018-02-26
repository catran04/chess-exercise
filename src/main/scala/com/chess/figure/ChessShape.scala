package com.chess.figure

/**
  * Created by Administrator on 2/20/2018.
  */
trait ChessShape {
  def brokenField(horizontal: Int, vertical: Int, otherHorizontal: Int, otherVertical: Int): Boolean

  val priority: Int

  var placeHorizontal: Int = _
  var placeVertical: Int = _

}
