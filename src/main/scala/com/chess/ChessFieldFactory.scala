package com.chess

import com.chess.figure.ChessShape

import scala.collection.mutable

/**
  * Created by Administrator on 2/21/2018.
  */
class ChessFieldFactory(width: Int, height: Int) {

  def create(figure: ChessShape,
             chessField: Array[Array[Int]] = createDefault(width, height)): Array[Array[Int]] = {
    for(i <- 0 until chessField.length) {
      for(j <- 0 until chessField(i).length) {
        if(chessField(i)(j) == 0) create()
      }
    }

  }

  private def createDefault(widht: Int, height: Int): Array[Array[Int]] = {
    var chessField: mutable.MutableList[Array[Int]] = new mutable.MutableList[Array[Int]]()
    for(i <- 0 until widht) {
      chessField += Array[Int](height)
    }
    chessField.toArray
  }
}

object ChessFieldFactory {
  def apply(width: Int, height: Int): ChessFieldFactory = new ChessFieldFactory(width, height)
}
