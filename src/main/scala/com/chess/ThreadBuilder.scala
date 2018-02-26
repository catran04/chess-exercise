package com.chess

import com.chess.figure.ChessShape

import scala.collection.mutable

/**
  * Created by Administrator on 2/22/2018.
  */
object ThreadBuilder {

  def createWorker(horizontal: Int, vertical: Int, setFigures: Array[ChessShape], chessField: (Int, Int)): Int = {
    if(setFigures.isEmpty) throw new RuntimeException("setFiguresIsEmpty")
    val sortedCollectionFigures = setFigures.sortBy(figure => figure.priority)
    var count: Int = 0
    new Thread() {
      override def run(): Unit = {
        setFigures(0).placeHorizontal = horizontal
        setFigures(0).placeVertical = vertical


//        var usedFigures: mutable.MutableList[ChessShape] = mutable.MutableList[ChessShape](setFigures(0))

        abcd(sortedCollectionFigures, chessField, 1)

        def abcd(sortedCollectionFigures: Array[ChessShape], chessField: (Int, Int), numberFigures: Int): Unit = {
          for(hor <- sortedCollectionFigures(numberFigures).placeHorizontal to chessField._1){
            for(vert <- sortedCollectionFigures(numberFigures).placeVertical to chessField._2) {
              sortedCollectionFigures(numberFigures).placeHorizontal = hor
              sortedCollectionFigures(numberFigures).placeVertical = vert
              if(!isBrokenPlace(sortedCollectionFigures.take(numberFigures - 1), sortedCollectionFigures(numberFigures))) {
                if(numberFigures + 1 == sortedCollectionFigures.length) {
                  count += 1
                }else {
                  abcd(sortedCollectionFigures, chessField, numberFigures + 1)
                }
                if(hor == chessField._1 && vert == chessField._2) {
                  if(numberFigures - 1 == sortedCollectionFigures(numberFigures)) {
                    return
                  }
//                  usedFigures += sortedCollectionFigures(numberFigures)
                  if(numberFigures != 0) {
                    abcd(sortedCollectionFigures, chessField, numberFigures - 1)
                  }
                }
              }
              if(hor == chessField._1 && vert == chessField._2) {
                if(numberFigures + 1 == sortedCollectionFigures.length) {
//                  usedFigures = usedFigures.dropRight(1)
                  abcd(sortedCollectionFigures, chessField, numberFigures - 1)
                }
                abcd(sortedCollectionFigures, chessField, numberFigures + 1)
              }
            }
          }
        }
      }
    }.start()
    println(s"COUNT ${count}")
    count
  }


  /**
    * checks brokes places
    *
    * @param usedFigures: Array[ChessShape] - The list of shapes on the chess field
    * @param currentFigure: ChessShape - The current figure
    * @return Boolean- returns true if at least one figure brokes other figure
    */
  private def isBrokenPlace(usedFigures: Array[ChessShape], currentFigure: ChessShape): Boolean = {
    for(i <- usedFigures.indices) {
      if(usedFigures(i).brokenField(currentFigure.placeHorizontal, currentFigure.placeVertical) ||
      currentFigure.brokenField(usedFigures(i).placeHorizontal, usedFigures(i).placeVertical)) return true
    }
    false
  }
}
