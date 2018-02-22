package com.chess.figure

import scala.collection.mutable

/**
  * Created by Administrator on 2/20/2018.
  */
class Queen extends ChessShape {

  def place(horizontal: Int, vertical: Int, chessField: Array[Array[Int]]): Array[Array[Int]] = {
    for (i <- chessField.indices) {
      for (j <- chessField(i).indices) {
        Array[Array[Int]]
      }
    }
  }
}

object Queen {
  def apply(): Queen = new Queen()
}