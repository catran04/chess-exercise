package com.chess

import com.chess.figure._

/**
  * Created by Administrator on 2/20/2018.
  */
object FigureFactory {
  def createKing : ChessShape = new King

  def createQueen: ChessShape = new Queen

  def createCastle: ChessShape = new Castle

  def createBishop: ChessShape = new Bishop

  def createKnight: ChessShape = new Knight
}
